



(*** GAME CONTROL AND DESIGN "CONSTANTS" ***)

    (* player *)
let _initial_player_pos = (300,25);;
let _player_size = (50,25);;
let _player_step_distance = 20;;
let _player_boundaries = (20,870);;

    (* bullet *)
let _bullet_speed = 0.05;;
let _bullet_size = (5,10);;
let _bullet_step_distance = 10;;

    (* enemy *)
let _enemies_lines = 4;;
let _enemies_rows = 8;;
let _enemy_speed = 0.2;;
let _enemy_size = (25,25);;
let _enemy_step_distance = 20;;
let _enemy_downstep_distance = 30;;
let _enemy_area_border = (20,870) ;;
let _first_enemy_pos = (20, 550);;
let _space_between_enemies = ((fst _enemy_size)+45, (snd _enemy_size)+25);;


(*** TYPES ***)
type game_state =
  {
    player : int * int;
    enemies : (int * int) list;
    bullet : int * int;
    enemy_delay : float;
    bullet_delay : float;
    enemy_speed : float;
    bullet_speed : float;
    enemy_direction : bool;
    bullet_on : bool;
  
  }