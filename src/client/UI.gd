extends Control


@export var cur_turn_label:Label

# Called when the node enters the scene tree for the first time.
func _ready():
	CommBus.game_state_updated.connect(_on_game_state_updated)
	CommBus.player_turn_available.connect(_on_player_turn_update)


func _on_game_state_updated():
	cur_turn_label.text = "Current Turn: "+str(Global.current_turn)

func _on_player_turn_update(avail):
	if avail:
		cur_turn_label.modulate = Color(1.0, 1.0, 1.0, 1.0)
	else:
		cur_turn_label.modulate = Color(1.0, 0.0, 0.0, 1.0)
