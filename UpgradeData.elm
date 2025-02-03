module UpgradeData exposing
  ( cost
  , updateIorin
  , updateDelta
  , description
  )

import Array exposing (Array)
import BigInt exposing (..)

cost : Int -> Int -> BigInt
cost n level =
  case n of
    0 -> fromInt 0
    1 -> mul (fromInt 10) <| pow (fromInt 5) (fromInt level)
    2 -> pow (fromInt 10) (fromInt (level+2))
    3 -> pow (fromInt 100) (fromInt level)
    4 -> fromInt 0
    _ -> fromInt 0

updateDelta : Int -> Int -> BigInt -> Array Int -> (BigInt, Array Int)
updateDelta n level oldDelta arr =
  (
    case n of
      0 -> oldDelta
      1 -> add oldDelta (fromInt 1)
      2 -> add oldDelta (fromInt 10)
      3 -> oldDelta
      _ -> fromInt 0
  , arr
  )

updateIorin : Int -> Int -> BigInt -> Array Int-> (BigInt, Array Int)
updateIorin n level oldIorin arr =
  case n of
    0 -> (add oldIorin (fromInt 1), arr)
    1 -> (oldIorin,arr)
    2 -> (oldIorin,arr)
    3 -> (mul oldIorin (fromInt 2),arr)
    4 -> (fromInt 0, Array.repeat 5 0)
    _ -> (oldIorin,arr)

description : Int -> Int -> String
description n level =
  case n of
    0 -> "Iorin ++"
    1 -> "Delta ++   / cost : "++toString (cost n level)++" / level : "++String.fromInt level
    2 -> "Delta += 10/ cost : "++toString (cost n level)++" / level : "++String.fromInt level
    3 -> "Iorin * 2  / cost : "++toString (cost n level)++" / level : "++String.fromInt level
    4 -> "RESET(EXPERIMENT)"
    _ -> ""


-- UTILITIES

getMathTo left right =

getStepwisePowerSum level base stepLength offset =
  let
    intPart = div level stepLength
    modPart = sub level (mul intPart stepLength)
    d = div stepLength (sub base (fromInt 1))
  in
    mul (add d modPart) (pow base intPart)
      |> add (negate d)
      |> add offset