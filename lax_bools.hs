class LaxBool a where
    laxbool :: a -> Bool

instance LaxBool Int where
    laxbool 0 = False
    laxbool _ = True

instance LaxBool [a] where
    laxbool [] = False
    laxbool _  = True

instance LaxBool Bool where
    laxbool = id

instance (LaxBool a) => LaxBool (Maybe a) where
    laxbool Nothing  = False
    laxbool (Just a) = laxbool a
