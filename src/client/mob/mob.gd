class_name Mob
extends Node2D

var _definition: MobDefinition
var backgorund:ColorRect
var front_tile:Sprite2D


func setup(grid_position: Vector2i, mob_definition: MobDefinition) -> void:
	backgorund = $Background
	front_tile = $FrontGlyph
	position = Grid.grid_to_world(grid_position)
	set_definition(mob_definition)

func set_definition(definition: MobDefinition) -> void:
	_definition = definition
	front_tile.texture = _definition.texture
	front_tile.self_modulate = _definition.front_color
	backgorund.self_modulate = _definition.back_color
