extends Node

class_name GameState

@onready var socket:WebSocketClient = $"WebSocket"

func _ready():
	socket.on_connected.connect(game_state_connected)
	socket.on_message.connect(game_state_message)
	
func game_state_connected():
	socket.send_as_json(["request-game-state"])

func game_state_message(msg):
	if len(msg) == 0:
		push_error("Incorrect msg. Expected length > 0.")
		return

	var cmd = msg[0]
	if cmd == "game-state":
		if (len(msg) == 1):
			push_error("Incorrect msg :game-state. Expected body. ", msg)
			return
			
		var dict:Dictionary = msg[1]
		# update mobs
		_update_mobs(dict)
		
		# update level terrain
		_update_terrain(dict)
		
		CommBus.emit_signal("game_state_updated")

func _update_mobs(dict):
	Global.mobs = dict.get("mobs")
	
func _update_terrain(dict):
	Global.terrain.clear()
	
	if dict.has("level"):
		var max_x:int = dict["level"]["max-x"] if dict["level"].has("max-x") else null
		var max_y:int = dict["level"]["max-y"] if dict["level"].has("max-y") else null
		var cur_turn:int = dict["level"]["cur-turn"] if dict["level"].has("cur-turn") else null
		
		Global.level_max_x = max_x
		Global.level_max_y = max_y
		Global.current_turn = cur_turn
		
		if dict["level"].has("memo"):
			for i in len(dict["level"]["memo"]):
				var y = i / max_x
				var x = i % max_x
				
				var info = dict["level"]["memo"][i]
				info["x"] = x
				info["y"] = y
				
				var v = true if info["v"] == "true" else false
				info["v"] = v
				
				var r = true if info["r"] == "true" else false
				info["r"] = r
				
				Global.terrain[str(x)+"x"+str(y)] = info
