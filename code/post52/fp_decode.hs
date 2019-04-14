#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.7
  --package containers
-}


import Control.Monad (forM_)
import Data.IntMap.Strict (IntMap, fromList, (!?))
import System.Environment (getArgs)


data InterpreterResult = NotInterpreted
                       | InterpreterOk String
                       | InterpreterError String

type MessageID = Int
type MessageStatus = Int

newtype Interpreter = Interpreter { getInterpreter :: MessageID -> MessageStatus -> InterpreterResult }


create_group_interpreter :: Int            -- range start
                         -> Int            -- range end (exclusive)
                         -> IntMap String  -- the status map for the group
                         -> String         -- the name of the group
                         -> Interpreter
create_group_interpreter range_start range_end status_map group_name =
  let
    interpreter id status =
      if (range_start <= id) && (id < range_end)
      then
        let device_nr = id - range_start in
        case (status_map !? status) of
          Nothing -> InterpreterError ("Unknown status " ++ (show status) ++
                                       " for " ++ group_name ++ " #" ++ (show device_nr))
          Just s -> InterpreterOk (group_name ++ " #" ++ (show device_nr) ++ ": " ++ s)
      else
        NotInterpreted
  in
    Interpreter interpreter


motor_status_map = fromList [ (0, "Not moving")
                            , (1, "Moving")
                            , (2, "Hardware defect") ]
motor_interpreter = create_group_interpreter 20 30 motor_status_map "Motor"

sensor_status_map = fromList [ (0, "Ok")
                             , (1, "Above threshold")
                             , (2, "Below threshold") ]
sensor_interpreter = create_group_interpreter 40 50 sensor_status_map "Sensor"

gpio_status_map = fromList [ (0, "Off")
                           , (1, "On") ]
gpio_interpreter = create_group_interpreter 50 58 gpio_status_map "GPIO"


device_a_interpreter_func id status =
  let
    statusMap = fromList [ (0, "Idle")
                         , (1, "Processing")
                         , (2, "Sending") ]
  in
    if (id == 2)
    then
      case (statusMap !? status) of
        Nothing -> InterpreterError ("Unknown status " ++ (show status) ++ " for Device_A")
        Just s -> InterpreterOk ("Device_A: " ++ s)
    else
      NotInterpreted

device_a_interpreter = Interpreter device_a_interpreter_func


device_b_interpreter_func id status =
  let
    statusMap = fromList [ (0, "Idle")
                         , (1, "Calculating") ]
  in
    if (id == 4)
    then
      case (statusMap !? status) of
        Nothing -> InterpreterError ("Unknown status " ++ (show status) ++ " for Device_B")
        Just s -> InterpreterOk ("Device_B: " ++ s)
    else
      NotInterpreted

device_b_interpreter = Interpreter device_b_interpreter_func

-- main application logic starts here

interpreters = [ motor_interpreter
               , sensor_interpreter
               , gpio_interpreter
               , device_a_interpreter
               , device_b_interpreter
               ]


decodeWithInterpreters :: [Interpreter] -> MessageID -> MessageStatus -> InterpreterResult
decodeWithInterpreters interpreters id status =
  let tryInterpreter (Interpreter curr) next =
        case (curr id status) of
          NotInterpreted -> next
          result -> result
  in foldr tryInterpreter NotInterpreted interpreters


decode = decodeWithInterpreters interpreters


onOk :: String -> IO ()
onOk s = putStrLn s

onErr :: String -> IO ()
onErr s = putStrLn ("Error! " ++ s)


handleResult err ok res =
  case res of
    NotInterpreted -> return ()
    InterpreterOk s -> ok s
    InterpreterError s -> err s


main = do
  fileName <- head <$> getArgs
  fileContent <- readFile fileName
  let messages = map read (lines fileContent) :: [(Int,Int)]
      printResult = handleResult onErr onOk
  forM_ messages (printResult . (uncurry decode))
