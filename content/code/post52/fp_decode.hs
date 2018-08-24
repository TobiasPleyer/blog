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


createMotorInterpreter :: Int -> String -> Interpreter
createMotorInterpreter motorID motorName =
  let
    motorNetworkID = motorID + 20
    statusMap = fromList [ (0, "Not moving")
                         , (1, "Moving")
                         , (2, "Hardware defect") ]
    interpreter mID mStatus =
      if (mID == motorNetworkID)
      then
        case (statusMap !? mStatus) of
          Nothing -> InterpreterError ("Unknown status " ++ (show mStatus) ++ " for motor " ++ motorName)
          Just s -> InterpreterOk ("Motor " ++ motorName ++ ": " ++ s)
      else
        NotInterpreted
  in
    Interpreter interpreter


createSensorInterpreter :: Int -> String -> Interpreter
createSensorInterpreter sensorID sensorName =
  let
    sensorNetworkID = sensorID + 40
    statusMap = fromList [ (0, "Ok")
                         , (1, "Above threshold")
                         , (2, "Below threshold") ]
    interpreter sID sStatus =
      if (sID == sensorNetworkID)
      then
        case (statusMap !? sStatus) of
          Nothing -> InterpreterError ("Unknown status " ++ (show sStatus) ++ " for sensor " ++ sensorName)
          Just s -> InterpreterOk ("Sensor " ++ sensorName ++ ": " ++ s)
      else
        NotInterpreted
  in
    Interpreter interpreter


createGpioInterpreter :: Int -> String -> Interpreter
createGpioInterpreter gpioID gpioName =
  let
    gpioNetworkID = gpioID + 50
    statusMap = fromList [ (0, "Off")
                         , (1, "On") ]
    interpreter gID gStatus =
      if (gID == gpioNetworkID)
      then
        case (statusMap !? gStatus) of
          Nothing -> InterpreterError ("Unknown status " ++ (show gStatus) ++ " for gpio " ++ gpioName)
          Just s -> InterpreterOk ("GPIO " ++ gpioName ++ ": " ++ s)
      else
        NotInterpreted
  in
    Interpreter interpreter


deviceAInterpreterFunc deviceID deviceStatus =
  let
    statusMap = fromList [ (0, "Idle")
                         , (1, "Processing")
                         , (2, "Sending") ]
  in
    if (deviceID == 2)
    then
      case (statusMap !? deviceStatus) of
        Nothing -> InterpreterError ("Unknown status " ++ (show deviceStatus) ++ " for Device_A")
        Just s -> InterpreterOk ("Device_A: " ++ s)
    else
      NotInterpreted

deviceAInterpreter = Interpreter deviceAInterpreterFunc


deviceBInterpreterFunc deviceID deviceStatus =
  let
    statusMap = fromList [ (0, "Idle")
                         , (1, "Calculating") ]
  in
    if (deviceID == 4)
    then
      case (statusMap !? deviceStatus) of
        Nothing -> InterpreterError ("Unknown status " ++ (show deviceStatus) ++ " for Device_B")
        Just s -> InterpreterOk ("Device_B: " ++ s)
    else
      NotInterpreted

deviceBInterpreter = Interpreter deviceBInterpreterFunc


motors = [ (1, "Motor_A")
         , (2, "Motor_B")
         , (3, "Motor_C")]


sensors = [ (1, "Sensor_A")
          , (2, "Sensor_B")]


gpios = [ (1, "GPIO_A")
        , (2, "GPIO_B")]


interpreters = [createMotorInterpreter  i n | (i,n) <- motors ] ++
               [createSensorInterpreter i n | (i,n) <- sensors] ++
               [createGpioInterpreter   i n | (i,n) <- gpios  ] ++
               [deviceAInterpreter, deviceBInterpreter]


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
