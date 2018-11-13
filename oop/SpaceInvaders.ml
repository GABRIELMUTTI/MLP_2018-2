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
  let pl = new player {x = 300; y = 50} in

  (* TELA INICIAL *)
  initial_screen screen;
  screen#setState _ingame_state;
  
  (* LOOP PRINCIPAL *)
  while true do

    (* PEGA EVENTO *)
    let event = Graphics.wait_next_event [ Graphics.Poll ] in
        if event.Graphics.keypressed then
          pl#setKey (read_key ())
        else 
          pl#setKey '0'
    ;

    (* UPDATES *)
    pl#update;


    (* DESENHOS *)
    auto_synchronize false;
    screen#draw;
    pl#draw;
    synchronize ()
  done;
  
  (* TELA FINAL *)
  screen#draw;
  ignore(Graphics.wait_next_event [ Key_pressed ]);
  screen#closeW;
;;

  
let _ = main ()