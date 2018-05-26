namespace Maze_game
open System
open FSharpx
open SDL2
open OpenTK
open Prime
open Prime.Stream
open Prime.Chain
open Nu




// this is the game dispatcher that is customized for our game. In here, we create screens and wire
// them up with subsciptions and transitions.
type Maze_game_Dispatcher () =
    inherit GameDispatcher ()
    
    override dispatcher.Register (_, world) =
        // TODO: start by creating and wiring up your game's screens in here! For an example, look
        // at BlazeDispatcher.fs in the BlazeVector project.
        world

// this is a plugin for the Nu game engine by which user-defined dispatchers, facets, and other
// sorts of types can be obtained by both your application and Gaia.
type Maze_game_Plugin () =
    inherit NuPlugin ()

    // make our game-specific game dispatcher...
    //override this.MakeGameDispatcherOpt () =
    //    Some (Maze_game_Dispatcher () :> GameDispatcher)

    // specify the above game dispatcher to use
    //override this.GetStandAloneGameDispatcherName () =
    //    typeof<Maze_game_Dispatcher>.Name
    
// this is the main module for our program.
module Program =

    let rec build_wall_ls (structure_in:char list) (acc:bool list) : bool list =
        match structure_in with
        | [] -> acc
        | '0'::xs -> build_wall_ls xs (List.append acc [false; false; false; false])
        | '1'::xs -> build_wall_ls xs (List.append acc [false; false; false; true])
        | '2'::xs -> build_wall_ls xs (List.append acc [false; false; true; false])
        | '3'::xs -> build_wall_ls xs (List.append acc [false; false; true; true])
        | '4'::xs -> build_wall_ls xs (List.append acc [false; true; false; false])
        | '5'::xs -> build_wall_ls xs (List.append acc [false; true; false; true])
        | '6'::xs -> build_wall_ls xs (List.append acc [false; true; true; false])
        | '7'::xs -> build_wall_ls xs (List.append acc [false; true; true; true])
        | '8'::xs -> build_wall_ls xs (List.append acc [true; false; false; false])
        | '9'::xs -> build_wall_ls xs (List.append acc [true; false; false; true])
        | 'A'::xs -> build_wall_ls xs (List.append acc [true; false; true; false])
        | 'B'::xs -> build_wall_ls xs (List.append acc [true; false; true; true])
        | 'C'::xs -> build_wall_ls xs (List.append acc [true; true; false; false])
        | 'D'::xs -> build_wall_ls xs (List.append acc [true; true; false; true])
        | 'E'::xs -> build_wall_ls xs (List.append acc [true; true; true; false])
        | 'F'::xs -> build_wall_ls xs (List.append acc [true; true; true; true])

    let rec rebox_wall_ls (wall_ls_in:bool list) (wall_ls_out:(bool list) list) (u:int) (v:int) (u_limit:int) (v_limit:int) : (bool list) list =
        if u = u_limit && v = v_limit then List.rev ((wall_ls_in |> Seq.take 2 |> List.ofSeq) :: wall_ls_out)
        elif u = 0 && v = 0 then rebox_wall_ls (wall_ls_in |> Seq.skip 4 |> List.ofSeq) ((wall_ls_in |> Seq.take 4 |> List.ofSeq) :: wall_ls_out) (u + 1) v u_limit v_limit
        elif v = 0 && u < u_limit then rebox_wall_ls (wall_ls_in |> Seq.skip 3 |> List.ofSeq) ((wall_ls_in |> Seq.take 3 |> List.ofSeq) :: wall_ls_out) (u + 1) v u_limit v_limit
        elif v = 0 && u = u_limit then rebox_wall_ls (wall_ls_in |> Seq.skip 3 |> List.ofSeq) ((wall_ls_in |> Seq.take 3 |> List.ofSeq) :: wall_ls_out) 0 (v + 1) u_limit v_limit
        elif u = 0 then rebox_wall_ls (wall_ls_in |> Seq.skip 3 |> List.ofSeq) ((wall_ls_in |> Seq.take 3 |> List.ofSeq) :: wall_ls_out) (u + 1) v u_limit v_limit
        elif u = u_limit then rebox_wall_ls (wall_ls_in |> Seq.skip 2 |> List.ofSeq) ((wall_ls_in |> Seq.take 2 |> List.ofSeq) :: wall_ls_out) 0 (v + 1) u_limit v_limit
        else rebox_wall_ls (wall_ls_in |> Seq.skip 2 |> List.ofSeq) ((wall_ls_in |> Seq.take 2 |> List.ofSeq) :: wall_ls_out) (u + 1) v u_limit v_limit

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

    

        // initialize Nu
        Nu.init false

        // this specifies the window configuration used to display the game.
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "$safeprojectname$" }
        
        // this specifies the configuration of the game engine's use of SDL.
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // this is a callback that attempts to make 'the world' in a functional programming
        // sense. In a Nu game, the world is represented as an abstract data type named World.
        let attemptMakeWorld sdlDeps =

            // an instance of the above plugin
            let plugin = Maze_game_Plugin ()

            // here is an attempt to make the world with the various initial states, the engine
            // plugin, and SDL dependencies.
            World.attemptMake true 1L () plugin sdlDeps

        // after some configuration it is time to run the game. We're off and running!
        World.run attemptMakeWorld id id sdlConfig