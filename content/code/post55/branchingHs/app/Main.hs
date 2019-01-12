import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Writer.Lazy
import Control.Monad (liftM)
import Data.String.Utils
import System.Exit
import System.IO
import System.Process hiding (shell)


type MyStack a = EitherT String (WriterT [String] IO) a


shell :: String -> MyStack String
shell cmd = do
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "/bin/zsh" ["-c", cmd] []
  if (exitCode == ExitSuccess)
  then EitherT $ writer (Right (stdout), ["Executing '" ++ cmd ++ "'", stdout])
  else EitherT $ writer (Left (stdout), ["Executing '" ++ cmd ++ "'", stdout, stderr])

act_on_branch b =
  if (b == "master")
  then shell "echo 'action for master'"
  else shell "echo 'Other branch'"

stackReturn value = return $ return $ return (Right value, [])

main = do
  (res,info) <- runWriterT $ runEitherT $ do
    bOut <- shell "git branch"
    let branch = last $ words bOut
    act_on_branch branch
    shell "echo 'command independent of previous commands'"
    shell $ "echo " ++ branch
    cnt <- strip <$> shell "git tag | wc -l"
    shell $ "for i in {1.." ++ cnt ++ "}; do; echo 'Branch!'; done"
  case res of
    Right _ -> putStrLn "SUCCESS"
    Left _ -> putStrLn "FAILURE"
  print res
  putStrLn "== INFO =="
  mapM_ putStrLn info
