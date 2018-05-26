namespace Maze_game

open Nu

module Maze_game_dispatchers =
  open Prime.ReflectionModule

  type Entity with
      member this.SetPosition u v world = this.Set Property? Position [u; v] world
  

