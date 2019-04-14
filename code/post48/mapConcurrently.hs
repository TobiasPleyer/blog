mapConcurrently myAction [data1, data2, data3]
-- definition of mapConcurrently
  = runConcurrently . traverse (Concurrently . myAction) [data1, data2, data3]
-- definition of traverse for lists
  = runConcurrently (
      List.foldr (\x ys -> liftA2 (:) ((Concurrently . myAction) x) ys)
                 (pure []) [data1, data2, data3]
    )
-- definition of liftA2 for lists
  = runConcurrently (
      List.foldr (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys)
                 (pure []) [data1, data2, data3]
    )
-- definition of foldr evaluated strictly
  = runConcurrently (
      (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data1 (
        (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data2 (
          (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys)
            data3 (pure [])
        )
      )
    )
-- definition of `pure` for Concurrently
  = runConcurrently (
      (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data1 (
        (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data2 (
          (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys)
            data3 (Concurrently (return []))
        )
      )
    )
-- lambda application
  = runConcurrently (
      (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data1 (
        (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data2 (
          (<*>) (Concurrently ((:) <$> myAction data3)) (Concurrently (return []))
        )
      )
    )
-- definition of <*> for Concurrently
  = runConcurrently (
      (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data1 (
        (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data2 (
          Concurrently ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data3) (return []))
        )
      )
    )
-- lambda application and definition of <*> for Concurrently
  = runConcurrently (
      (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data1 (
        (<*>) (fmap (:) ((Concurrently . myAction) data2)) (Concurrently ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data3) (return [])))
      )
    )
-- definition of fmap for Concurrently
  = runConcurrently (
      (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data1 (
        (<*>) (Concurrently ((:) <$> myAction data2)) (Concurrently ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data3) (return [])))
      )
    )
-- definition of <*> for Concurrently
  = runConcurrently (
      (\x ys -> (<*>) (fmap (:) ((Concurrently . myAction) x)) ys) data1 (
        Concurrently ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data2) ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data3) (return [])))
      )
    )
-- lambda application and definition of <*> for Concurrently
  = runConcurrently (Concurrently ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data1) ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data2) ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data3) (return [])))))
-- apply runConcurrently (unpack)
  = (\(f,a) -> f a) <$> concurrently ((:) <$> myAction data1) ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data2) ((\(f,a) -> f a) <$> concurrently ((:) <$> myAction data3) (return [])))
-- myAction dataX = IO resX
  = (\(f,a) -> f a) <$> concurrently ((:) <$> (IO res1)) ((\(f,a) -> f a) <$> concurrently ((:) <$> (IO res2)) ((\(f,a) -> f a) <$> concurrently ((:) <$> (IO res3)) (return [])))
-- 'execute' IO actions and apply definition of `concurrently`
  = (\(f,a) -> f a) <$> concurrently ((:) <$> (IO res1)) ((\(f,a) -> f a) <$> concurrently ((:) <$> (IO res2)) ((\(f,a) -> f a) <$> (IO ((:) res3, []))))
-- definition of `concurrently`
  = (\(f,a) -> f a) <$> concurrently ((:) <$> (IO res1)) ((\(f,a) -> f a) <$> concurrently ((:) <$> (IO res2)) (IO ((:) res3 [])))
-- definition of `concurrently`
  = (\(f,a) -> f a) <$> concurrently ((:) <$> (IO res1)) ((\(f,a) -> f a) <$> concurrently ((:) <$> (IO res2)) (IO [res3]))
-- ...
  = [res1, res2, res3]
