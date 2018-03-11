module Tennis
open System

type Point = Love | Fifteen | Thirty | Fourty

type Player = Player1 | Player2

type Normal = {
    P1: Point
    P2: Point
    }

type State = Normal of Normal | Advantage of Player | Game of Player // c=20

type ScoreFunction = State -> Player -> State

let getNextPoint point =
    match point with
    | Love -> Fifteen
    | Fifteen -> Thirty
    | Thirty -> Fourty

let addPoint player state =
    match player with
    | Player1 ->
        { state with P1= getNextPoint state.P1 }
    | Player2 ->
        { state with P2= getNextPoint state.P2 }

let readableScore score =
    match score with
    | Love -> "0"
    | Fifteen -> "15"
    | Thirty -> "30"
    | Fourty -> "40"

let log state =
    let str = 
        match state with
        | Normal a ->
            sprintf "Player 1: %s, Player 2: %s" (readableScore a.P1) (readableScore a.P2)
        | Advantage a ->
            match a with
            | Player1 -> "Player 1 has advantage"
            | Player2 -> "Player 2 has advantage"
        | Game g ->
            match g with
            | Player1 -> "PLAYER 1 WINS"
            | Player2 -> "PLAYER 2 WINS"
    
    printfn "%s" str

let score state player =
    let newState = 
        match state with
        | Normal { P1=Fourty; P2=Fourty } -> Advantage player
        | Normal { P1=Fourty } when player=Player1 -> Advantage Player1
        | Normal { P2=Fourty } when player=Player2 -> Advantage Player2
        | Normal s ->
            Normal (addPoint player s)
        | Advantage a ->
            match a with
            | Player1 when player = Player1 -> Game Player1
            | Player1 -> Normal { P1=Fourty; P2=Fourty }
            | Player2 when player = Player2 -> Game Player2
            | Player2 -> Normal { P1=Fourty; P2=Fourty }
        | Game g -> Game g
    log newState
    newState

let readInput (player: string) state =
    match player.ToLower() with
    | "player1" | "p1" -> score state Player1
    | "player2" | "p2" -> score state Player2
    | _ -> state

let rec driver state =
    match state with
    | Game g -> printfn "******"
    | _ ->
        printf "Enter player: "
        let input = Console.ReadLine()
        driver (readInput input state)

let start =
    let initialState = Normal {
        P1=Love
        P2=Love
    }
    driver initialState
