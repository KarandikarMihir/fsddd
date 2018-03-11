module Bowling

open System.Text.RegularExpressions

type Score = Undefined | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
type Frame = {
    Roll1: Score
    Roll2: Score
    }

type FrameType = Normal | Spare | Strike
type ScoreBoard = {
    ScoreSoFar: int
    Bonus: int
    CurrentFrame: Frame
    }

let pointToInt point =
    match point with
    | Undefined -> 0
    | Zero -> 0
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10

let intToPoint point =
    match point with
    | 0 -> Zero
    | 1 -> One
    | 2 -> Two
    | 3 -> Three
    | 4 -> Four
    | 5 -> Five
    | 6 -> Six
    | 7 -> Seven
    | 8 -> Eight
    | 9 -> Nine
    | 10 -> Ten

let getCharList (str: string) = (Regex.Replace(str, @"\s+", "")).ToCharArray() |> Array.toList

let changeRoll score point =
    if point = Ten
    then
        { score with CurrentFrame = { Roll1 = Undefined; Roll2 = Undefined } }
    else
        match score with
            | { CurrentFrame = { Roll1 = Undefined } } ->
                { score with CurrentFrame = { Roll1 = point; Roll2 = Undefined } }
            | { CurrentFrame = { Roll2 = Undefined } } ->
                { score with CurrentFrame = { Roll1 = Undefined; Roll2 = point } }

let convertPoints ch frame =
    match ch with
    | 'X' | 'x' -> 10
    | '/' -> 10 - (pointToInt frame.Roll1)
    | '-' -> 0
    | _ ->
        let x = System.Convert.ToString(ch)
        System.Int32.Parse(x)

let log (state: ScoreBoard) (frames: FrameType list) = printfn "***\n%A\n%A\n***" state frames

let calculatePoints ch state =
    let points = convertPoints ch state.CurrentFrame
    if state.Bonus > 0
    then ({ state with Bonus = state.Bonus - 1 }, points, points)
    else (state, points, 0)

let rec folder chars state frames =

    // terminate when we have 10 frames after calculating the bonus
    log state frames
    if List.length frames = 10
    then
        match chars with
        | ch::tail ->
            match ch with
            | 'X' | 'x' ->
                let bonusState = { state with ScoreSoFar = state.ScoreSoFar + 10 }
                folder tail bonusState frames
            | '-' -> folder tail state frames
            | _ ->
                let points = convertPoints ch state.CurrentFrame
                let bonusState = { state with ScoreSoFar = state.ScoreSoFar + points }
                folder tail bonusState frames
        | _ -> state
    else

    match chars with
    | ch::tail ->
        let (newState, points, bonus) = calculatePoints ch state
        match ch with
        | 'X' | 'x' -> 
            let score = {
                newState with
                    ScoreSoFar = newState.ScoreSoFar + points + bonus
                    Bonus = newState.Bonus + 2
                }
            folder tail (changeRoll score Ten) (frames @ [Strike])
        | '/' ->
            let score = {
                newState with
                    ScoreSoFar = newState.ScoreSoFar + points + bonus
                    Bonus = newState.Bonus + 1
                }
            folder tail (changeRoll score Ten) (frames @ [Spare])
        | '-' ->
            let newScore = changeRoll newState (intToPoint points)
            let newFrame =
                if newScore.CurrentFrame.Roll2 <> Undefined
                then [Normal]
                else []
            folder tail newScore (frames @ newFrame)
        | _ ->
            let score = { newState with ScoreSoFar = newState.ScoreSoFar + points }
            let newScore = changeRoll score (intToPoint points)
            let newFrame =
                if newScore.CurrentFrame.Roll2 <> Undefined
                then [Normal]
                else []
            folder tail newScore (frames @ newFrame)
    | _ -> state

let parse str =
    let score = {
        ScoreSoFar = 0
        Bonus = 0
        CurrentFrame = { Roll1 = Undefined; Roll2 = Undefined }
        }
    folder (getCharList str) score []
