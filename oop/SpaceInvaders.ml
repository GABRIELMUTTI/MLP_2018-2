open GameObject;;
open Bullet;;
open Ship;;
open Enemy;;
open Player;;
open Config;;
open Graphics;;
open Screen;;


let  initial_screen screen = 

  screen#draw;
  ignore(Graphics.wait_next_event [ Key_pressed ]);
;;

let  main () =
  
  let screen = new screen in
  screen#openW;


  (*   CRIA OBJETOS PLAYER, INIMIGOS *)
  let player = new player {x = 300; y = 25} in

  (* TELA INICIAL *)
  initial_screen screen;
  screen#setState _ingame_state;

  (* LOOP PRINCIPAL *)
  while (screen#getState != _lose_state && screen#getState != _win_state) do

    (* PEGA EVENTO *)
    let event = Graphics.wait_next_event [ Graphics.Poll ] in
        if event.Graphics.keypressed then
          player#setKey (read_key ())
        else 
          player#setKey '0'
    ;

    (* UPDATES *)
    player#update;


    (* DESENHOS *)
    auto_synchronize false;
    screen#draw;
    player#draw;
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