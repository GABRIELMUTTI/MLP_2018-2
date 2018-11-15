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
  let player = new player {x = 300; y = 25} in
  let bullet = new bullet {x = 0; y = 0} 0 in
  let enemies = build_enemies () in

  (* TELA INICIAL *)
  initial_screen screen;
  screen#setState _ingame_state;

  (* LOOP PRINCIPAL *)

  let old_time = ref (get_time_now ()) in
  let new_time = ref 0.0 in
  let dt = ref 0.0 in
  while (screen#getState != _lose_state && screen#getState != _win_state) do

    (* PEGA EVENTO *)
    let event = Graphics.wait_next_event [ Graphics.Poll ] in
        if event.Graphics.keypressed then
          match (read_key ()) with
          | 'a' -> player#setKey 'a'
          | 'd' -> player#setKey 'd'
          | ' ' -> if(not bullet#getOn) then 
                    begin bullet#setOn true; 
                          bullet#setPosition 
                            ({player#getPosition 
                                with x = player#getPosition.x+22} ) 
                        end 
                   else ()
          | _ -> ()
        else
          ()
    ;

    new_time :=  get_time_now ();
    dt :=  !new_time -. !old_time; 
    old_time := !new_time;
    (* UPDATES *)
    player#update !dt;
    bullet#update !dt;
    changeDirection enemies;
    List.iter (fun x -> x#update !dt) enemies;
    
    


    (* DESENHOS *)
    auto_synchronize false;
    screen#draw;
    bullet#draw;
    player#draw;
    List.iter (fun x -> x#draw) enemies;
    
    synchronize ();

    
    (* testa fim de jogo*)
    if player#getLife < 0 then screen#setState _lose_state;
    
  done;
  
  (* TELA FINAL *)
  screen#draw;
  ignore(Graphics.wait_next_event [ Key_pressed ]);
  screen#closeW;
;;

  
let _ = main ()