;;;; aoee-server.lisp

(in-package #:aoee-server)

(defparameter *rwlock* (aoee-server/websocket::make-rw-lock))

(defparameter *game-loop-thread* nil)
(defparameter *game-loop-cond-var* (bt2:make-condition-variable))


(defun process-game-loop ()
  (loop with l = (bt2:make-lock)
        with any-moved = nil
        do
           (alexandria:cswitch ((get-game-loop-state))
             (+game-loop-init+ (log:info "*game-loop-state* = +GAME-LOOP-INIT+, -> +GAME-LOOP-PLAYER-TURN+~%")
                               (setf any-moved nil)
                               (aoee-server/websocket::with-write-lock *rwlock*
                                 ;; TODO: solve issue when the player is null 
                                 (update-visible-map *player*)
                                 (broadcast-game-state)
                                 
                                 ;; skip player turn if no player is available or player can not act
                                 (if (or (null *player*)
                                         (skip-turn-p *player*))
                                     (set-game-loop-state +game-loop-init+ +game-loop-other-turn+)
                                     (set-game-loop-state +game-loop-init+ +game-loop-player-turn+))))
             (+game-loop-player-turn+ (bt2:with-lock-held (l)
                                        ;; player turn here, *game-loop-state* moves forward from client requests 
                                        (loop while (= (get-game-loop-state) +game-loop-player-turn+)
                                              do
                                                 (bt2:condition-wait *game-loop-cond-var* l)
                                                 (setf any-moved t))))
             (+game-loop-other-turn+ (aoee-server/websocket::with-write-lock *rwlock*
                                       (log:info "*game-loop-state* = +GAME-LOOP-OTHER-TURN+~%")
                                       ;; do all AI here
                                       (loop for mob-id in (mob-id-list *level*)
                                             for mob = (get-mob-by-id mob-id)
                                             when (and (not (eq mob *player*))
                                                       (not (skip-turn-p mob)))
                                             do
                                                (ai-function mob)
                                                (setf any-moved t)
                                                (broadcast-game-state))
                                       (log:info "*game-loop-state* +GAME-LOOP-OTHER-TURN+ -> +GAME-LOOP-FINALIZE-TURN+~%")
                                       (set-game-loop-state +game-loop-other-turn+ +game-loop-finalize-turn+)))
             (+game-loop-finalize-turn+ (aoee-server/websocket::with-write-lock *rwlock*
                                          (log:info "*game-loop-state* = +GAME-LOOP-FINALIZE-TURN+~%")

                                          (when (null any-moved)
                                            (log:info "Nobody can act any more, finalizing turn...")
                                            (loop for mob-id in (mob-id-list *level*)
                                                  for mob = (get-mob-by-id mob-id)
                                                  do
                                                   (incf (ap mob) +max-ap+)))
                                          (log:info "*game-loop-state* +GAME-LOOP-FINALIZE-TURN+ -> +GAME-LOOP-INIT-TURN+~%")
                                          (set-game-loop-state +game-loop-finalize-turn+ +game-loop-init+))))))

(defun translate-game-state-to-json ()
  (with-slots (mob-id-list) *level*
    (yason:with-output-to-string* ()
      (yason:with-array ()
        (yason:encode-array-element :game-state)
        (yason:with-object ()
          (yason:encode-object-element :player (id *player*))
          (yason:encode-object-element :level *level*)
          (yason:with-object-element (:mobs)
            (yason:with-array ()
              (loop for mob-id in mob-id-list
                    for mob = (get-mob-by-id mob-id)
                    do (yason:encode-array-element mob)))))))))

(defun broadcast-game-state ()
  (aoee-server/websocket:broadcast-to-all-clients (translate-game-state-to-json)))

(defun send-game-state (websocket)
  (let ((json (translate-game-state-to-json)))
    ;;(log:info "Game state requested.~%~A" json)
    (aoee-server/websocket:send-msg websocket json)))

(defun handle-move-cmd (client msg)
  (let* ((dir (str-to-keyword (gethash :dir (second msg))))
         (can-move-result (mob-can-move-p *player* *level* dir))
         (move-result (mob-move *player* *level* dir :mob-can-move-p-result can-move-result)))

    move-result))

(defun process-user-cmd (msg client)
  (when (< (length msg) 2)
    (log:error "Expected params after :user-cmd, got nil.")
    (return-from process-user-cmd))
  (let* ((params (second msg)))
    
    (unless (eq (type-of params) 'hash-table)
      (log:error "Expected params as a dictionary (hash-table) after :user-cmd, got ~A." (type-of params))
      (return-from process-user-cmd))
    
    (let ((cmd-type (str-to-keyword (gethash :type params))))

     (case cmd-type
        (:move (progn
                 (handle-move-cmd client msg)))
        (t (progn
             (log:error "Unexpected :type in :user-cmd params, got ~A." cmd-type)
             (return-from process-user-cmd)))))))

(defun process-client-message (client message)
  (log:info "Raw msg received: " message)
  
  (let ((parsed-msg (yason:parse message)))
    (when (null parsed-msg)
      (log:error "Parsed message empty.")
      (return-from process-client-message))
    
    (let ((cmd (str-to-keyword (first parsed-msg)))
          (write-operation nil)
          (read-operation nil))
      (case cmd
        (:request-game-state (setf read-operation (lambda () 
                                                    (send-game-state client))))
        (:user-cmd (setf write-operation (lambda ()
                                           (process-user-cmd parsed-msg client)))))
      
      (unless (null write-operation)
        (aoee-server/websocket::with-write-lock *rwlock*
          ;; the write lock was released in another thread, and this thread has acquired the lock
          ;; but we need to check that another thread has not moved the game loop forward
          (when (/= (bt2:atomic-integer-value *game-loop-state*) +game-loop-player-turn+)
            (return-from process-client-message))
          (when (funcall write-operation)
            (update-visible-map *player*)
            (broadcast-game-state)
            (set-game-loop-state +game-loop-player-turn+ +game-loop-other-turn+)
            (bt2:condition-notify *game-loop-cond-var*)
            (log:info "*game-loop-state* +GAME-LOOP-PLAYER-TURN+ -> +GAME-LOOP-OTHER-TURN+~%")))
        (return-from process-client-message))
      
      (unless (null read-operation)
        (aoee-server/websocket::with-read-lock *rwlock*
          (funcall read-operation))
        (return-from process-client-message)))))

(defun process-client-connect (client)
  (declare (ignore client))
  ;; start game loop once the first client connects
  (when (= (length aoee-server/websocket::*connections*) 1)
    (process-game-loop)))

(defun define-client-server-communication ()
  (aoee-server/websocket:define-handlers-for-url "/"
    :open (lambda (client)
            )
    :message (lambda (client message)
               (process-client-message client message)
               ))
  )

(defun start-server ()
  "Starts a AoEE websocket server."

  (setf *level* (make-level))
  (set-terrain-* *level* 2 2 :terrain-wall)
  (set-terrain-* *level* 3 2 :terrain-wall)
  
  (adjust-array *mobs* 0)
  (let ((player (make-mob :mob-type-player)))
    (setf (x player) 1 (y player) 1)
    (setf *player* player)
    (add-mob-to-level *level* *player*))

  (let ((mob (make-mob :mob-type-imp)))
    (setf (x mob) 5 (y mob) 5)
    (add-mob-to-level *level* mob))

  (setf *rwlock* (aoee-server/websocket::make-rw-lock))
  (reset-game-loop-state)
  
  (define-client-server-communication)

  (when (or (null *game-loop-thread*) 
            (not (bt2:thread-alive-p *game-loop-thread*)))
    (setf *game-loop-thread* (bt2:make-thread #'process-game-loop :name "game loop thread")))
  (aoee-server/websocket:start-server 32167))

(defun stop-server ()
  "Stops AoEE websocket server."

  (when (and (not (null *game-loop-thread*))
             (bt2:thread-alive-p *game-loop-thread*))
    (bt2:destroy-thread *game-loop-thread*))
  (aoee-server/websocket:stop-server))
