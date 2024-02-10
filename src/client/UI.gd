extends Control


@export var cur_turn_label:Label

# Called when the node enters the scene tree for the first time.
func _ready():
	CommBus.game_state_updated.connect(_on_game_state_updated)


func _on_game_state_updated():
	cur_turn_label.text = "Current Turn: "+str(Global.current_turn)
