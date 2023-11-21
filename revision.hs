bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "you're underweight , you emo, you"
    | bmi <= 25.0 = "you'r ugly"
    | bmi <= 30 = "You're fat! Lose some weight, fatty!"
    | otherwise = "you're a whale, congratulations"