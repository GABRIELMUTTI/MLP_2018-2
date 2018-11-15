
type coords =
  {
    x : int;
    y : int;
  }


(*** GAME CONTROL AND DESIGN "CONSTANTS" ***)

let _screen_size = (900, 600);;
let _game_over_line = 100;;

    (* player *)
let _initial_player_pos = (300,25);;
let _player_size = (50,25);;
let _player_step_distance = 20;;
let _player_boundaries = (20,870);;
let _player_life = 3;;
let _hit_flash_speed = 1.0;;

    (* bullet *)
let _bullet_speed = 0.02;;
let _bullet_size = (5,10);;
let _bullet_step_distance = 10;;

    (* enemy *)
let _enemies_lines = 4;;
let _enemies_rows = 8;;
let _enemy_speed = 0.2;;
let _enemy_size = (27,25);;
let _enemy_step_distance = 20;;
let _enemy_downstep_distance = 10;;
let _enemy_area_border = (20,840) ;;
let _first_enemy_pos = (20, 550);;
let _space_between_enemies = ((fst _enemy_size)+45, (snd _enemy_size)+25);;
let _enemy_firerate = 1.0;;
                      

(* screen states*)
let _initial_state = 0;;
let _ingame_state = 1;;
let _lose_state = 2;;
let _win_state = 3;;