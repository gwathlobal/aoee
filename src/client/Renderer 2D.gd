extends Node2D

var tile_types:Dictionary = {}
const UNREVEALED_TILE = "unrevealed"

@onready var level_container = $Level

const tile_prefab = preload("res://map/tile.tscn")

# Called when the node enters the scene tree for the first time.
func _ready():
	CommBus.game_state_updated.connect(_on_game_state_updated)
	
	# load tile definitions
	var dir:PackedStringArray = DirAccess.get_files_at("res://assets/definitions/tile/")
	for filename in dir:
		filename = filename.trim_suffix(".remap")
		var tile:TileDefinition = ResourceLoader.load("res://assets/definitions/tile/" + filename)
		tile_types[tile.id] = tile

func _on_game_state_updated():
	_update_tiles()

func _update_tiles():
	# remove all tiles
	for e in level_container.get_children():
		var terrain = e as Tile
		terrain.hide()
		terrain.queue_free()
	
	# add new tiles
	for key in Global.terrain.keys():
		var info = Global.terrain[key] as Dictionary
		
		var tile = tile_prefab.instantiate()
		var tile_position := Vector2i(info["x"], info["y"])
		
		var tile_type
		if info["r"]: 
			if info.get("m"):
				tile_type = tile_types[info["m"]]
			elif info.get("t"):
				tile_type = tile_types[info["t"]]
		else:
			tile_type = tile_types[UNREVEALED_TILE]
			
		tile.setup(tile_position, tile_type, info["v"])
		tile.name = str(info["t"]) + "_" + str(info["x"]) + "x" + str(info["y"])
		
		level_container.add_child(tile)
