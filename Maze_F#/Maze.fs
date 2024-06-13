(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Gfx
open System.Text
open Engine
 
//Classe maze
type maze (w, h) =

    //variabile che contiene la posizione attuale
    let mutable (posizione_attuale: int*int) = 0,0
    
    //variabile che contiene la matrice di coppie di bool inizializzate a (false,false)
    let mutable (lab: (bool*bool)[,]) = (Array2D.create w h (false,false)) //il primo elemento indica se la cella è visitata, il secondo se è un muro 

    //lista che simula lo stack, inizialmente lista vuota, utlizzato per salvare le posizioni nell'ordine in cui vengono visitate
    let mutable (stack_pos: (int*int)list) = []

    //variabile che conta il numero di celle visitate
    let mutable (count_visited: int) = 0

    //variabile che salva la direzione valida
    let mutable (conf_move:int) = 0

    //costante che contiene la posizione iniziale
    let pos_iniziale = (0,0) //la partenza del player è in alto a sinistra

    //costante che contiene la posizione finale
    let posizione_finale = ((w-2),(h-1))
       
    //funzione che controlla se una casella esiste e in tal caso se non è già stata visitata
    member private this.is_avaiable (riga, colonna) = 
        if ((riga<0) || (riga > (h-1)) || (colonna<0) || (colonna > (w - 1))) then false
        else match lab.[riga, colonna] with
              |(true,_) -> false
              |(false,_) -> true

    //funzione che controlla se una posizione è un un percorso
    member this.isPath(riga, colonna) = 
        match lab.[riga,colonna] with
        |(_,true) -> true
        |_ -> false    


    //funzione che calcola la posizione successiva in base ad una direzione (dettata da un int) -> 0=North, 1=East, 2=South, 3=West 
    member private this.next_pos(x: int) = 
        match x with
        |0 -> ((fst(posizione_attuale))-2), (snd(posizione_attuale))
        |1 -> (fst(posizione_attuale)), ((snd(posizione_attuale))+2)
        |2 -> ((fst(posizione_attuale))+2), (snd(posizione_attuale))
        |3 -> (fst(posizione_attuale)), ((snd(posizione_attuale))-2)
        |_ -> failwith "ERRORE, DIREZIONE IMPREVISTA"


    //funzione che randomizza un numero compreso tra 0 e 3 (SOLO NEI CASI POSSIBILI), che indica la direzione -> 0=North, 1=East, 2=South, 3=West
    member private this.rnd_dir () = 
        let rec generate_move() =
           let move = (rnd_int 0 3) 
           in if this.is_avaiable(this.next_pos(move)) then move //controlla se la nuova posizione (next_pos(move)) esiste e se non è già stata visitata
              else generate_move()
        in
        conf_move <- (generate_move()) //salva la direzione nella variabile conf_move, serve salvarla perchè verrà utilizzata per settare come Path la cella di mezzo 
        this.next_pos (conf_move)
           
           
    //funzione che calcola la posizione di mezzo dato lo spostamento randomico precedentemente generato
    member private this.middle_pos (x: int) = 
        match x with
        |0 ->  ((fst(posizione_attuale))+1), (snd(posizione_attuale)) //dato che nell'algoritmo la posizione attuale è già stata aggiornata, per settare come path la cella di mezzo è necessario
        |1 ->  (fst(posizione_attuale)), ((snd(posizione_attuale))-1) //effettuare un salto all' indietro di 1
        |2 ->  ((fst(posizione_attuale))-1), (snd(posizione_attuale)) 
        |3 ->  (fst(posizione_attuale)), ((snd(posizione_attuale))+1)  
        |_ -> failwith "ERRORE, DIREZIONE IMPREVISTA"

    //funzione che setta la cella di mezzo come path 
    member private this.set_middle_position () = 
        lab.[(fst(this.middle_pos(conf_move))), (snd(this.middle_pos(conf_move)))] <- (false,true)
                        
                     
    //funzione che controlla se è necessario il back tracking (se tutte le celle adiacenti sono o visitate o al di fuori della matrice): int*int->bool
    member private this.is_isolated(riga, colonna) = 
        (not(this.is_avaiable(riga,(colonna+2)))) && (not(this.is_avaiable(riga, (colonna-2)))) && (not(this.is_avaiable((riga+2), colonna))) && (not(this.is_avaiable((riga-2), colonna)))


    //funzione che riorsivamente trova la prima cella in cui non tutte le celle adiacenti sono visitate
    member private this.find_with_backtracking () = 
        let rec aux stack_pos = //funzione ausiliaria che scorre la lista ricorsivamente alla ricerca di una coppia in cui non è più necessario continuare il backtracking
            match stack_pos with
                |[] -> failwith "ERRORE, non è possibile che il contatore non abbia raggiunto il limite che non ci siano celle in cui non ci si possa muovere" 
                |x::xs -> if (this.is_isolated(x)) then (aux xs) //caso in cui è necessario un ulteriore passo indietro
                          else (x, (x::xs)) //è arrivato ad una coppia in cui ci sono ancora 1 o più celle adiacenti in cui potersi muovere e restituisce tale cella e lo stack aggiornato (rimuove il ramo cieco)  
        in aux stack_pos
     

    //ALGORITMO che modifica la lista di bool
    member this.generate() =
    
        posizione_attuale <- pos_iniziale // la posizione attuale viene settata come la posizione iniziale
    
        while (count_visited<(((w*h)/4)-1)) do //dato che il nostro algoritmo si basa su un movimento di 2 in 2, data una matrice pari il numero delle caselle da visitare sarà 1/4, in questo caso -1 perchè l'ultima verrà settata al di fuori del ciclo while
            lab.[fst(posizione_attuale), snd(posizione_attuale)] <- (true,true) //la cella corrispondente alla posizione attuale viene settata come path e come visitata
            stack_pos <- [posizione_attuale]@(stack_pos) //la coppia posizione attuale viene concatenata in testa alla lista contenete le posizioni visitate dall'algoritmo
            count_visited <- count_visited+1 //il contatore viene aggiornato
            if this.is_isolated(posizione_attuale) then //ramo in cui è necesario ricorrere al backtracking
                    posizione_attuale <- fst(this.find_with_backtracking()) //la posizione attuale viene settata sulla cella trovata dalla funzione find.with.backtracking 
                    stack_pos <- snd(this.find_with_backtracking()) //lo stack vienne settato elliminando il ramo cieco
                    posizione_attuale <- this.rnd_dir() //a questo punto cerca una nuova posizione disponibile e la salva nella posizione attuale
                    this.set_middle_position() //qui viene settato come path la cella di mezzo 
            else //ramo in cui non è necessario il backtracking
                    posizione_attuale <- this.rnd_dir() //posizione_attuale viene settato in base alla direzione randomica
                    this.set_middle_position() //la cella di mezzo viene settata come path 
        lab.[fst(posizione_attuale), snd(posizione_attuale)] <- (true,true) //alla fine del loop si setta come visitata e come path la cella finale, che è stata appositamente esclusa dal ciclo while perchè altrimenti l'algoritmo avrebbe ripetuto operazioni non più necesarie
        lab.[(w-2),(h-1)] <-(false,true) //l'uscita è nell'angolo in basso a destra                                                                   
    
    //funzione che risolve il labirinto
    member this.risolutore (posizione: int*int) = 
        //funzione che verifica se l'elemento e è presente nella lista l
        let rec isPresent e l =     
            match l with
                |[] -> false
                |x::xs -> (e=x) ||(isPresent e xs)     
                
        //funzione che riorsivamente trova la prima cella in cui non tutte le celle adiacenti sono visitate
        let rec backtrack aux_stack stack = //aux_stack è lo stack che viene decrementato ogni volta, mentre stack è quello totale (costante)
            //Log.debug "stack_backtrack: %A\naux_stack: %A" stack aux_stack //Stampa stack di backtrack
            match aux_stack with
            |[] -> failwith "ERRORE" 
            |(x,y)::xs -> if (x-1) >= 0 && this.isPath(x-1,y) && not(isPresent (x-1,y) stack) || 
                                   (y+1) >= 0 && this.isPath(x,y+1) && not(isPresent (x,y+1) stack) || 
                                   (x+1) >= 0 && this.isPath(x+1,y) && not(isPresent (x+1,y) stack) || 
                                   (y-1) >= 0 && this.isPath(x,y-1) && not(isPresent (x,y-1) stack) then
                                   //Log.debug "nuova_posizione: %A" (x,y) //Stampa della nuova posizione trovata
                                   (x,y)
                          else backtrack xs stack
        
        //funzione che data una lista, elimina i valori compresi tra due valori uguali
        let rec elimina_rami (lista:(int*int)list):(int*int)list = 
            let rec aux e ls =
                match ls with
                |[]->[]
                |x::xs-> if e = x then e::xs
                            else aux e xs
            in
            match lista with
            |[]->[]
            |x::xs-> if isPresent x xs then elimina_rami (aux x xs)
                        else x::(elimina_rami xs)

        //funzione che percorre la via più a destra possibile, quando non ha più celle disponibili, attiva il backtrack
        let rec ris x y stack =
            //Log.debug "stack_ris: %A" stack //Stampa stack totole
            if (x,y) = posizione_finale then [posizione_finale] //caso base
            elif(x-1) >= 0 && not(isPresent (x-1,y) stack) && this.isPath(x-1,y) //sinistra NOSTRA, destra del Maze
                then ((x-1),y)::(ris (x-1) y ((x-1,y)::stack)) 

            elif(y+1) >= 0 &&  not(isPresent (x,(y+1)) stack) && this.isPath(x,(y+1)) //sotto
                then (x,(y+1))::(ris x (y+1) ((x,y+1)::stack)) 

            elif (x+1) >= 0 && not(isPresent (x+1,y) stack) && this.isPath((x+1),y) //destra NOSTRA, sinistra del Maze
                then (x+1,y)::(ris (x+1) y ((x+1,y)::stack)) 

            elif (y-1) >= 0 && not(isPresent (x,y-1) stack) && this.isPath(x,(y-1)) //sopra
                then (x,y-1)::(ris x (y-1) ((x,y-1)::stack))

            else 
                let new_pos = backtrack stack stack //calcolo della nuova posizione in caso del backtracking
                in
                elimina_rami (new_pos::(ris (fst(new_pos)) (snd(new_pos)) (new_pos::stack))) //concateno la new_pos in modo che la funzione elimina_rami (se serve) elimini i valori di troppo calcolati in precedenza
        in
        elimina_rami (posizione::(ris (fst(posizione)) (snd(posizione)) [posizione])) //aggiunta la posizione del player allo stack di bella copia

      
[< NoEquality; NoComparison >]
type state = {
    player : sprite
}

let W = 92
let H = 50

let main () =
    let engine = new engine (W, H)
    let mutable w_maze = 0 //larghezza matrice del maze
    let mutable h_maze = 0 //altezza matrice del maze
    let mutable shift_matrix_y = 5 //numero di pixel(caratteri) da saltare per distanziare il maze dai bordi
    let mutable shift_matrix_x = 26
    let mutable player_pos = (0,0) //variabile che salva la posizione corrente del player
    
    //funzione per scegliere la difficoltà del livello
    let take_level (key: ConsoleKeyInfo) (screen: wronly_raster) (st: state) =
        let level = key.KeyChar
        match level with
        |'1'->w_maze <- 16
              h_maze <- 16
              shift_matrix_x <- 39
              shift_matrix_y <- 18
        |'2'->w_maze <- 20
              h_maze <- 20
              shift_matrix_x <- 37
              shift_matrix_y <- 18
        |'3'->w_maze <- 30
              h_maze <- 30
              shift_matrix_x <- 32
              shift_matrix_y <- 10
        |'4'-> 
              w_maze <- 40
              h_maze <- 40
              shift_matrix_x <- 27
              shift_matrix_y <- 5
        |_-> ()
    
        st, ((key.KeyChar = '1') || (key.KeyChar = '2') || (key.KeyChar = '3') || (key.KeyChar = '4'))
    

    //sprite per gestire il menu in grafica ASCII
    let menu = engine.create_and_register_sprite(image.rectangle(W, H, pixel.filled Color.Black, pixel.filled Color.Black), 0, 1, -100)
    menu.draw_text("   ▄▄▄▄███▄▄▄▄      ▄████████  ▄███████▄     ▄████████    ▄████████  ▄████████  ▄█   ▄█  ", 2, 5, Color.Red)
    menu.draw_text(" ▄██▀▀▀███▀▀▀██▄   ███    ███ ██▀     ▄██   ███    ███   ███    ███ ███    ███ ███  ███  ", 2, 6, Color.Red)
    menu.draw_text(" ███   ███   ███   ███    ███       ▄███▀   ███    █▀    ███    █▀  ███    █▀  ███▌ ███▌ ", 2, 7, Color.Red)
    menu.draw_text(" ███   ███   ███   ███    ███  ▀█▀▄███▀▄▄  ▄███▄▄▄       ███        ███        ███▌ ███▌ ", 2, 8, Color.Red)
    menu.draw_text(" ███   ███   ███ ▀███████████   ▄███▀   ▀ ▀▀███▀▀▀     ▀███████████ ███        ███▌ ███▌ ", 2, 9, Color.Red)
    menu.draw_text(" ███   ███   ███   ███    ███ ▄███▀         ███    █▄           ███ ███    █▄  ███  ███  ", 2, 10, Color.Red)
    menu.draw_text(" ███   ███   ███   ███    ███ ███▄     ▄█   ███    ███    ▄█    ███ ███    ███ ███  ███  ", 2, 11, Color.Red)
    menu.draw_text("  ▀█   ███   █▀    ███    █▀   ▀████████▀   ██████████  ▄████████▀  ████████▀  █▀   █▀   ", 2, 12, Color.Red)

    menu.draw_text(" ____  ____  ____  ___  ___      __      _  _  ____  _  _   ", 15, 20, Color.Red)
    menu.draw_text("(  _ \(  _ \( ___)/ __)/ __)    /__\    ( )/ )( ___)( \/ )()",15, 21, Color.Red)
    menu.draw_text(" )___/ )   / )__) \__ \\__ \   /(__)\    )  (  )__)  \  /   ",15, 22, Color.Red)
    menu.draw_text("(__)  (_)\_)(____)(___/(___/  (__)(__)  (_)\_)(____) (__) ()",15, 23, Color.Red)
    
    menu.draw_text("/|   .    _  |  _       |  _     _ |    /|  ", 22, 28, Color.Red)
    menu.draw_text(" |   .   |_) | (_| \/   | (- \/ (- |     |  ", 22, 29, Color.Red)
    menu.draw_text("         |         /                        ", 22, 30, Color.Red)
    menu.draw_text("__                                      __  ", 22, 32, Color.Red)
    menu.draw_text(" _)  .    _  |  _       |  _     _ |     _) ", 22, 33, Color.Red)
    menu.draw_text("/__  .   |_) | (_| \/   | (- \/ (- |    /__ ", 22, 34, Color.Red)
    menu.draw_text("         |         /                        ", 22, 35, Color.Red)
    menu.draw_text("__                                      __  ", 22, 37, Color.Red)
    menu.draw_text(" _)  .    _  |  _       |  _     _ |     _) ", 22, 38, Color.Red)
    menu.draw_text("__)  .   |_) | (_| \/   | (- \/ (- |    __) ", 22, 39, Color.Red)
    menu.draw_text("         |         /                        ", 22, 40, Color.Red)
    
    menu.draw_text("|__| .    _  |  _       |  _     _ |   |__| ", 22, 43, Color.Red)
    menu.draw_text("   | .   |_) | (_| \/   | (- \/ (- |      | ", 22, 44, Color.Red)
    menu.draw_text("         |         /                        ", 22, 45, Color.Red)

    let st0 = {
        player = menu // controllo
    }
    //loop per scelta del livello
    engine.loop_on_key take_level st0
   
    
    //Creazione del maze
    let maze = new maze (w_maze,h_maze)
    let posizione_finale = ((w_maze-2),(h_maze-1))
    maze.generate() 
    
    //funzione che printa la soluzione
    let rec print stack = 
        match stack with
        |[] -> ()
        |x::xs -> ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Green, pixel.filled Color.Green), (fst(x)+shift_matrix_x), (snd(x)+shift_matrix_y), 0)
                  print xs 


    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.
        
        
        if (key.KeyChar = 'r') then //caso in cui il giocatore vuole il risolutore
            print (maze.risolutore(player_pos))
        elif (player_pos = posizione_finale) then
            let youwin = engine.create_and_register_sprite (image.rectangle (42, 5, pixel.filled Color.Black, pixel.filled Color.Black), 26, 45, 100) //grafica ASCII per la fine
            youwin.draw_text("__  __                        _       __",1 ,0 , Color.Red)
            youwin.draw_text("\ \/ /___  __  __   _      __(_)___  / /",1 ,1 , Color.Red)
            youwin.draw_text(" \  / __ \/ / / /  | | /| / / / __ \/ / ",1 ,2 , Color.Red)
            youwin.draw_text(" / / /_/ / /_/ /   | |/ |/ / / / / /_/  ",1 ,3 , Color.Red)
            youwin.draw_text("/_/\____/\__,_/    |__/|__/_/_/ /_(_)   ",1 ,4 , Color.Red)
        else //controllo collisioni
            let dxi = int dx
            let dyi = int dy
            let try_move = (((fst(player_pos))+dxi),((snd(player_pos))+dyi)) //aggiunge dx dy a player pos
            if fst(try_move) >= 0 && snd(try_move) >= 0 && maze.isPath (try_move)
                 then st.player.move_by (dx,dy)
                      player_pos <- try_move    //conferma il movimento
                 else st.player.move_by (0,0)   //annulla il movimento
        
        st, key.KeyChar = 'q'

     
    //creo layer nero che copra il menu
    ignore <| engine.create_and_register_sprite(image.rectangle(W, H, pixel.filled Color.Black, pixel.filled Color.Black), 0, 1, -50)
       
    //creazione del maze (avrà solo il bordo destro e inferiore)
    for i in 0 .. (w_maze-1) do
        for j in 0 .. (h_maze-1) do
            if not(maze.isPath(i,j)) then 
                ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red, pixel.filled Color.Red), (i+shift_matrix_x), (j+shift_matrix_y), 0)
            else ()

    //creazione del bordo sinistro e superiore
    for j in 0 .. (w_maze-1) do
        ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red, pixel.filled Color.Red), (shift_matrix_x-1), (shift_matrix_y+j), 0)

    for i in -1 .. (w_maze-1) do
        ignore <| engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red, pixel.filled Color.Red), (shift_matrix_x+i), (shift_matrix_y-1), 0)

    //creazione del rettangolo che elenca i comandi
    let commands = engine.create_and_register_sprite (image.rectangle (42, 5, pixel.filled Color.Black, pixel.filled Color.Black), 28, 0, 0)
    commands.draw_text("W, A, S, D: move your player            ",1 ,0 , Color.Red)
    commands.draw_text("r: show the solution (FOR CHEATERS)     ",1 ,1 , Color.Red)
    commands.draw_text("q: quit the game                        ",1 ,2 , Color.Red)

    //creazione del player   
    let player = engine.create_and_register_sprite (image.rectangle (1,1, pixel.filled Color.White, pixel.filled Color.Gray),shift_matrix_x, shift_matrix_y,0)//shift_matrix, shift_matrix, 0  -> è come farlo partire dalla posizione 0,0 del maze

    // initialize state
    let st0 = { 
        player = player
        }
    // start engine
    engine.loop_on_key my_update st0