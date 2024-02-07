extends Node

class_name WebSocketClient

@export var websocket_url = "ws://127.0.0.1:32167"

var _socket:WebSocketPeer = WebSocketPeer.new()
@onready var _timer:Timer = $Timer

signal on_connected
signal on_message(message)
signal on_disconnected

enum STATES {
	DISCONNECTED,
	CONNECTING,
	CONNECTED
}

func _ready():
	_socket.set_inbound_buffer_size(655350)

func send_as_json(msg:Variant):
	var ready_state = _socket.get_ready_state()
	if ready_state == WebSocketPeer.STATE_OPEN:
		var json = JSON.stringify(msg)
		_socket.send_text(json)
		print("Msg sent: ", json)
	else:
		push_error("Socket not open. Unable to send message: ", msg)

var _state:STATES = STATES.DISCONNECTED

func _state_disconncted_to_connecting():
	if _state == STATES.DISCONNECTED:
		_state = STATES.CONNECTING

func _state_connecting_to_disconnected():
	if _state == STATES.CONNECTING:
		_state = STATES.DISCONNECTED

func _state_connecting_to_connected():
	if _state == STATES.CONNECTING:
		_state = STATES.CONNECTED 

func _state_connected_to_disconnected():
	if _state == STATES.CONNECTED:
		_state = STATES.DISCONNECTED

func _process(delta):
	if _state == STATES.CONNECTING:
		_socket.poll()
		var ready_state = _socket.get_ready_state()
		if ready_state == WebSocketPeer.STATE_OPEN:
			print("Connected to ", websocket_url)
			emit_signal("on_connected")
			_state_connecting_to_connected()
	elif _state == STATES.CONNECTED:
		_socket.poll()
		var ready_state = _socket.get_ready_state()
		if ready_state == WebSocketPeer.STATE_OPEN:
			while _socket.get_available_packet_count():
				var msg = _socket.get_packet().get_string_from_utf8()
				var msg_parsed = JSON.parse_string(msg)
				print("Msg received: ", msg_parsed)
				emit_signal("on_message", msg_parsed)
				
		elif ready_state == WebSocketPeer.STATE_CLOSED:
			var code = _socket.get_close_code()
			var reason = _socket.get_close_reason()
			print("WebSocket closed with code: %d, reason %s. Clean: %s" % [code, reason, code != -1])
			emit_signal("on_disconnected")
			_state_connected_to_disconnected()

func _on_timer_timeout():
	if _state == STATES.DISCONNECTED:
		print("Connecting to ", websocket_url, "...")
		var error = _socket.connect_to_url(websocket_url)
		if error == OK:
			_state_disconncted_to_connecting()
		else:
			print("Failed to connect! Error code: ", error)
	elif _state == STATES.CONNECTING:
		_socket.poll()
		var ready_state = _socket.get_ready_state()
		if ready_state == WebSocketPeer.STATE_CLOSED:
			var error = _socket.connect_to_url(websocket_url)
			if error != OK:
				print("Failed to connect! Error code: ", error)
				_state_connecting_to_disconnected()
	
