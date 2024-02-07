class_name Tile
extends Node2D

var _definition: TileDefinition
var _visible: bool
var backgorund:ColorRect
var front_tile:Sprite2D



func setup(grid_position: Vector2i, tile_definition: TileDefinition, visible:bool) -> void:
	backgorund = $Background
	front_tile = $FrontGlyph
	position = Grid.grid_to_world(grid_position)
	_visible = visible
	_definition = tile_definition
	front_tile.texture = _definition.texture
	if _visible:
		front_tile.self_modulate = _definition.front_color
	else:
		front_tile.self_modulate = Color(0.5, 0.5, 0.5, 1.0)
	backgorund.self_modulate = _definition.back_color
