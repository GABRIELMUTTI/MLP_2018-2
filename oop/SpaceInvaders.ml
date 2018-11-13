open GameObject;;
open Bullet;;
open Ship;;
open Enemy;;
open Player;;
open Config;;
open Graphics;;





let  main () =
  
  open_graph " 900x600";
  (*   CRIA OBJETOS TELA, PLAYER, INIMIGOS *)
  let pl = new player {x = 50; y = 50} in

  (* TELA INICIAL *)

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
    pl#draw;
  done
  
  (* TELA FINAL *)
;;

  
let _ = main ()