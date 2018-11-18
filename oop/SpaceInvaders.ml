open GameObject;;
open Bullet;;
open Ship;;
open Enemy;;
open Player;;
open Config;;
open Graphics;;
open Screen;;
open Utilities;;



let  initial_screen screen = 

  screen#draw;
  ignore(Graphics.wait_next_event [ Key_pressed ]);
;;



let  main () =
  
  let screen = new screen in
  screen#openW;


  (*   CRIA OBJETOS PLAYER, INIMIGOS *)
  let player = ref (new player {x = 300; y = 25} _player_size 0.0 _player_step_distance) in
  let bullet = ref (new bullet {x = 0; y = 0} _bullet_size _bullet_speed _bullet_step_distance 0) in
  let enemies = ref (build_enemies ()) in

  _objects := List.cons (ref (!player :> game_object)) !_objects;
  _objects := List.cons (ref (!bullet :> game_object)) !_objects;
  List.iter (fun x -> _objects := List.cons (ref x : game_object ref) !_objects) ((!enemies :> game_object list));

  
  (* TELA INICIAL *)
  initial_screen screen;
  screen#setState _ingame_state;

  let old_time = ref (get_time_now ()) in
  let new_time = ref 0.0 in
  let dt = ref 0.0 in
  let enemy_fire_delay = ref 0.0 in
  (* LOOP PRINCIPAL *)
  while (screen#getState != _lose_state && screen#getState != _win_state) do

    (* PEGA EVENTO *)
    (let event = Graphics.wait_next_event [ Graphics.Poll ] in
        if event.Graphics.keypressed then
          match (read_key ()) with
          | 'a' -> !player#setKey 'a'
          | 'd' -> !player#setKey 'd'
          | ' ' -> if(not !bullet#getOn) then 
                     begin
                       bullet := (new bullet {x = !player#getPosition.x + 22; y = !player#getPosition.y + 20} _bullet_size _bullet_speed _bullet_step_distance 0);
                       !bullet#setOn true;
                       _objects := List.cons (ref (!bullet :> game_object)) !_objects;
                     end 
                   else ()
          | '\027' -> screen#setState _lose_state
          | _ -> ()
        else
          (););

    new_time :=  get_time_now ();
    dt :=  !new_time -. !old_time; 
    old_time := !new_time;

    (* UPDATES *)
  
    if !enemy_fire_delay > _enemy_firerate then
        let enemy = select_random_enemy _objects in 
            ignore(enemy_fire_delay := 0.0);
            _objects := 
              !_objects@[ref (new bullet {!enemy#getPosition with x = !enemy#getPosition.x + 12} _bullet_size _bullet_speed _bullet_step_distance 1 :> game_object)]
      else 
        ignore(enemy_fire_delay := !enemy_fire_delay +. !dt);
  


    checkPlayerBulletEnd !bullet;
    changeDirection !enemies;
    List.iter (fun x -> !x#update !dt) !_objects;

    enemies := List.filter (fun x -> not x#getRemove) !enemies;

    if List.length !enemies == 0 then
      screen#setState _win_state;
    
    
    _objects := List.filter (fun x -> not(checkEnemyBulletEnd !x) ) !_objects;

    (* DESENHOS *)
    auto_synchronize false;
    screen#draw;
    
    List.iter (fun i -> Thread.join i)
      (List.map (fun obj -> Thread.create !obj#draw ()) !_objects);
    
    synchronize ();

    if(((enemy_far_down !enemies)#getPosition).y <= _game_over_line)
      then screen#setState _lose_state
    ;
    
    (* testa fim de jogo*)
    if !player#getLife <= 0 then screen#setState _lose_state;
    
  done;
  
  (* TELA FINAL *)
  screen#draw;
  ignore(Graphics.wait_next_event [ Key_pressed ]);
  while (read_key ()) != '\027' do
    ignore(Graphics.wait_next_event [ Key_pressed ])
  done;
  screen#closeW
;;

  
let _ = main ()
