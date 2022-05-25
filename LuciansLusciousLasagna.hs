module LuciansLusciousLasagna (
    elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes
) where

expectedMinutesInOven = 40

preparationTimeInMinutes layers = layers*2

elapsedTimeInMinutes layers minutesInOven =
    (preparationTimeInMinutes layers) + minutesInOven