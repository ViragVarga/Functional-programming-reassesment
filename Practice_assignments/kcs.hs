rotatingSquare :: Signal Drawing
rotatingSquare = fmap (:[]) $ fmap sq rs
                    where
                        rs :: Signal Transform
                        rs = fmap rotate timeS
                        
                        sq :: Transform -> (Transform, Shape)
                        sq t = ( scale (point 0.5 0.5) <+>
                                translate (point 1.2 0.4) <+> t, square)