extends Node

@export var game_state:GameState

func _unhandled_input(event):
	if event is InputEventKey and event.is_pressed():
		var keycode:Key = event.get_keycode()
		if keycode == KEY_ESCAPE:
			get_tree().quit()
		elif keycode == KEY_RIGHT or keycode == KEY_KP_6:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"e"}])
		elif keycode == KEY_LEFT or keycode == KEY_KP_4:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"w"}])
		elif keycode == KEY_UP or keycode == KEY_KP_8:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"n"}])
		elif keycode == KEY_DOWN or keycode == KEY_KP_2:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"s"}])
		elif keycode == KEY_KP_5:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"c"}])
		elif keycode == KEY_KP_9:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"ne"}])
		elif keycode == KEY_KP_7:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"nw"}])
		elif keycode == KEY_KP_3:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"se"}])
		elif keycode == KEY_KP_1:
			game_state.socket.send_as_json(["user-cmd", {"type":"move", "dir":"sw"}])
		else:
			print( OS.get_keycode_string(event.keycode))
