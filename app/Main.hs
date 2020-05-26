module Main where

import Lib
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Game
import Debug.Trace

width, height :: Int
width = 800
height = 800
-- TODO that's ugly
toRasterCoord :: Float -> Float
toRasterCoord coord = (coord) * 0.5 * (fromIntegral width -1)


window :: Gloss.Display
window = Gloss.InWindow "Cube" (width, height) (100, 100) -- the last parameter is position of the window itself

background :: Gloss.Color
background = Gloss.black

type Point = (Float, Float)
type Line = Point

type Vector = (Float, Float, Float, Float)

data Vector3 = Vector3 Float Float Float deriving Show

data Vector4 = Vector4 Float Float Float Float deriving Show

data Quaternion = Quaternion Float Vector3 deriving Show
identityQ :: Quaternion
identityQ = Quaternion 1 (Vector3 0 0 0)

data Matrix = Matrix Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float Float deriving Show
identityM :: Matrix
identityM = Matrix 1 0 0 0
                   0 1 0 0
                   0 0 1 0
                   0 0 0 1
            
data Model = Model
   { paths :: [[Vector3]]
   , scaleAndCenterModel :: Matrix
   , rotateModel :: Quaternion
   , translateModel :: Matrix
   } deriving Show

data Controls = Controls
   { forward :: Bool
   , backward :: Bool
   , left :: Bool
   , right :: Bool
   , up :: Bool
   , down :: Bool
   , rotl :: Bool
   , rotr :: Bool
   , rotu :: Bool
   , rotd :: Bool
   , lmb :: Bool
   , unpressedLastPos :: (Float, Float)
   , pressedStartPos :: (Float, Float)
   , pressedCurrPos :: (Float, Float)
   } deriving Show

data Camera = Camera
   { rotateCameraX :: Quaternion
   , rotateCameraY :: Quaternion
   , translateCamera :: Matrix
   , frustum :: Matrix
   } deriving Show

data Game = Game
   { environment :: [Model]
   , player1 :: Model
   , controls :: Controls
   , camera1 :: Camera
   } deriving Show

toHomogeneous :: Vector3 -> Vector
toHomogeneous (Vector3 x y z) = (x, y, z, 1)

interpolant :: Point -> Point -> Line -> Float
interpolant p0 p1 l = d0 / (d0 - d1) where
    d0 = dotProductV2 p0 l
    d1 = dotProductV2 p1 l

lerp :: Vector -> Vector -> Float -> Vector
lerp (ax, ay, az, aw) (bx, by, bz, bw) f = (ax + f*(bx - ax), ay + f*(by - ay), az + f*(bz - az), aw + f*(bw - aw))

rotationQuaternion :: Vector3 -> Float -> Quaternion
rotationQuaternion _ 0 = identityQ
rotationQuaternion (Vector3 x y z) alpha =
   Quaternion (cos (0.5 * alpha)) (Vector3 (sin (0.5 * alpha) * x) (sin (0.5 * alpha) * y) (sin (0.5 * alpha) * z))

quaternionToMatrix :: Quaternion -> Matrix
quaternionToMatrix (Quaternion w (Vector3 x y z)) = let n = 2/(x*x+y*y+z*z+w*w)
                       in Matrix (1 - n*y*y - n*z*z) (n*x*y - n*z*w)     (n*x*z + n*y*w)     0
                                 (n*x*y + n*z*w)     (1 - n*x*x - n*z*z) (n*y*z - n*x*w)     0
                                 (n*x*z - n*y*w)     (n*y*z + n*x*w)     (1 - n*x*x - n*y*y) 0
                                 0                   0                   0                   1

vector3Sum :: Vector3 -> Vector3 -> Vector3
vector3Sum (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ax+bx) (ay+by) (az+bz)

scalarMultV3 :: Float -> Vector3 -> Vector3
scalarMultV3 s (Vector3 x y z) = Vector3 (s*x) (s*y) (s*z)

dotProductV2 :: Point -> Point -> Float
dotProductV2 (ax, ay) (bx, by) = ax*bx + ay*by

dotProductV3 :: Vector3 -> Vector3 -> Float
dotProductV3 (Vector3 ax ay az) (Vector3 bx by bz) = ax*bx + ay*by + az*bz

crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (Vector3 ax ay az) (Vector3 bx by bz) = Vector3 (ay*bz - az*by) (az*bx - ax*bz) (ax*by - ay*bx)

quaternionMult :: Quaternion -> Quaternion -> Quaternion
quaternionMult (Quaternion s1 v1) (Quaternion s2 v2) = Quaternion (s1*s2 - dotProductV3 v1 v2) (scalarMultV3 s1 v2 `vector3Sum` (scalarMultV3 s2 v1) `vector3Sum` (crossProduct v1 v2))

matrixMult :: Matrix -> Matrix -> Matrix
matrixMult (Matrix a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44) (Matrix b11 b12 b13 b14 b21 b22 b23 b24 b31 b32 b33 b34 b41 b42 b43 b44) =
   Matrix (a11*b11 + a12*b21 + a13*b31 + a14*b41) (a11*b12 + a12*b22 + a13*b32 + a14*b42) (a11*b13 + a12*b23 + a13*b33 + a14*b43) (a11*b14 + a12*b24 + a13*b34 + a14*b44)
          (a21*b11 + a22*b21 + a23*b31 + a24*b41) (a21*b12 + a22*b22 + a23*b32 + a24*b42) (a21*b13 + a22*b23 + a23*b33 + a24*b43) (a21*b14 + a22*b24 + a23*b34 + a24*b44)
          (a31*b11 + a32*b21 + a33*b31 + a34*b41) (a31*b12 + a32*b22 + a33*b32 + a34*b42) (a31*b13 + a32*b23 + a33*b33 + a34*b43) (a31*b14 + a32*b24 + a33*b34 + a34*b44)
          (a41*b11 + a42*b21 + a43*b31 + a44*b41) (a41*b12 + a42*b22 + a43*b32 + a44*b42) (a41*b13 + a42*b23 + a43*b33 + a44*b43) (a41*b14 + a42*b24 + a43*b34 + a44*b44)

matrixVectorMult :: Matrix -> Vector -> Vector
matrixVectorMult (Matrix a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44) (x, y, z, w) =
    (a11 * x + a12 * y + a13 * z + a14 * w, a21 * x + a22 * y + a23 * z + a24 * w, a31 * x + a32 * y + a33 * z + a34 * w, a41 * x + a42 * y + a43 * z + a44 * w)

getProjectionMatrix :: Float -> Float -> Float -> Float -> Matrix
getProjectionMatrix ratio alpha near far =
   Matrix (1/(ratio*tan(0.5*alpha))) 0                  0                        0
          0                          (1/tan(0.5*alpha)) 0                        0
          0                          0                  ((-near-far)/(near-far)) (2*far*near/(near-far))
          0                          0                  1                        0

initialState :: Game
initialState = Game
               { environment = [Model
                  { paths = []
                  --{ paths = [[Vector3 0 100 0, Vector3 40 60 0, Vector3 10 60 0, Vector3 60 10 0, Vector3 10 10 0, Vector3 10 (-20) 0, Vector3 (-10) (-20) 0, Vector3 (-10) 10 0, Vector3 (-60) 10 0, Vector3 (-10) 60 0, Vector3 (-40) 60 0]]
                  , scaleAndCenterModel = identityM
                  , rotateModel = identityQ
                  , translateModel = Matrix 1 0 0 300
                                            0 1 0 0
                                            0 0 1 500
                                            0 0 0 1
                  }]
               , player1 = Model
                  { paths = [[Vector3 100 100 100, Vector3 100 (-100) 100, Vector3 (-100) (-100) (100), Vector3 (-100) 100 (100)]
                            ,[Vector3 100 100 (-100), Vector3 100 (-100) (-100), Vector3 (-100) (-100) (-100), Vector3 (-100) 100 (-100)]
                            ,[Vector3 100 100 100, Vector3 (-100) 100 100, Vector3 (-100) 100 (-100), Vector3 100 100 (-100)]
                            ,[Vector3 100 (-100) 100, Vector3 (-100) (-100) 100, Vector3 (-100) (-100) (-100), Vector3 100 (-100) (-100)]
                            ,[Vector3 100 100 (100), Vector3 100 (-100) 100, Vector3 100 (-100) (-100), Vector3 100 100 (-100)]
                            ,[Vector3 (-100) 100 100, Vector3 (-100) (-100) 100, Vector3 (-100) (-100) (-100), Vector3 (-100) 100 (-100)]]
                  , scaleAndCenterModel = identityM
                  , rotateModel = identityQ
                  , translateModel = identityM
                  }
               , controls = Controls False False False False False False False False False False False (0, 0) (0, 0) (0, 0)
               , camera1 = Camera identityQ identityQ (Matrix 1 0 0 0
                                                              0 1 0 0
                                                              0 0 1 0
                                                              0 0 0 1) (getProjectionMatrix 1 (0.5*pi) 30 10000)
               }

main :: IO ()
main = Game.play window background 30 initialState render eventHandler stepper where
   render :: Game -> Gloss.Picture
   -- render (Game _ (Model _ mtrx) _ _ _ _ _) | trace ("render " ++ show mtrx) False = undefined
   render game = Gloss.color Gloss.green $ Gloss.pictures $ getGameLines game where
        getGameLines :: Game -> [Gloss.Picture]
        getGameLines (Game env player1 _ camera1@(Camera _ _ _ projMtx)) = concatMap (getModelLines $ projMtx `matrixMult` getViewMatrix camera1) (player1 : env) where
        getViewMatrix :: Camera -> Matrix
        getViewMatrix (Camera q1 q2 m1 _) = (quaternionToMatrix $ quaternionMult q2 q1) `matrixMult` m1
        getModelLines :: Matrix -> Model -> [Gloss.Picture]
        getModelLines matrix model@(Model paths _ _ _) = map getLine $ perspectiveDivisionAnd2dProjection . clipPaths . (transformPaths $ matrix `matrixMult` (getModelMatrix model)) $ paths
        getModelMatrix :: Model -> Matrix
        -- getModelMatrix (Model _ scaleCenter rotate translate) | trace ("getModelMatrix rotate: " ++ show rotate ++ " translate: " ++ show translate) False = undefined
        getModelMatrix (Model _ scaleCenter rotate translate) = translate `matrixMult` (quaternionToMatrix rotate) `matrixMult` scaleCenter

        transformPaths :: Matrix -> [[Vector3]] -> [[Vector]]
        transformPaths matrix paths = map (map $ transformVertex matrix) paths

        transformVertex :: Matrix -> Vector3 -> Vector
        transformVertex matrix vertex = matrixVectorMult matrix (toHomogeneous vertex)

        clipPaths :: [[Vector]] -> [[Vector]]
        clipPaths [] = []
        clipPaths (path:paths) =
            case clipAllPlanes path of [] -> clipPaths paths
                                       [x] -> clipPaths paths
                                       clipped -> clipped : clipPaths paths

        clipAllPlanes :: [Vector] -> [Vector]
        clipAllPlanes path = shiftAndClip . shiftAndClip . shiftAndClip $ path where
            shiftAndClip :: [Vector] -> [Vector]
            shiftAndClip path = clip $ flip $ clip $ flip $ orientate $ path

        orientate :: [Vector] -> [Vector]
        orientate [] = []
        orientate ((ax, ay, az, aw):xs) = (az, ax, ay, aw) : orientate xs

        flip :: [Vector] -> [Vector]
        flip [] = []
        flip ((ax, ay, az, aw):xs) = (-ax, ay, az, aw) : flip xs

        clip :: [Vector] -> [Vector]
        clip [] = []
        clip [x] = [] -- XXX to chyba nie jest mozliwe
        clip path@(first:xs) = clipRec path where
            clipRec :: [Vector] -> [Vector]
            clipRec [x] = clipCouple x first
            clipRec (x:y:xs) = clipCouple x y ++ clipRec (y:xs)

        clipCouple :: Vector -> Vector -> [Vector]
        clipCouple first second | inBounds first && inBounds second = [first, second]
                                | not (inBounds first || inBounds second) = []
                                | onEdge first = [first]
                                | onEdge second = []
                                | inBounds first = [first, clipped first second]
                                | inBounds second = [clipped first second]
                                where inBounds :: Vector -> Bool
                                      inBounds (ax, _, _, aw) = ax <= aw
                                      onEdge :: Vector -> Bool
                                      onEdge (ax, _, _, aw) = ax == aw
                                      clipped :: Vector -> Vector -> Vector
                                      -- clipped (ax, ay, az, aw) (bx, by, bz, bw) | trace ("clipped ax: " ++ show ax ++ " aw: " ++ show aw ++ " bx: " ++ show bx ++ " bw: " ++ show bw ++ " -> clipped cx: " ++ show cx ++ " cw: " ++ show cw) False = undefined where (cx, _, _, cw) = lerp first second $ interpolant (ax, aw) (bx, bw) (-1, 1)
                                      clipped first@(ax, _, _, aw) second@(bx, _, _, bw) = lerp first second $ interpolant (ax, aw) (bx, bw) (-1, 1)

        perspectiveDivisionAnd2dProjection :: [[Vector]] -> [[Point]]
        perspectiveDivisionAnd2dProjection paths = map (map $ toRasterSpace . perspectiveDivision) paths

        perspectiveDivision :: Vector -> Vector
        perspectiveDivision (ax, ay, az, aw) = (ax/aw, ay/aw, az/aw, 1)

        toRasterSpace :: Vector -> Point
        toRasterSpace (ax, ay, _, _) = (toRasterCoord ax, toRasterCoord ay)

        getLine :: [Point] -> Gloss.Picture
        getLine path = Gloss.line $ closePath path

        closePath :: [Point] -> [Point]
        closePath [x, y] = [x, y] -- line
        closePath (x:xs) = x:xs ++ [x]

   eventHandler :: Game.Event -> Game -> Game
   -- eventHandler (Game.EventMotion (x, y)) _ | trace ("eventHandler " ++ show x ++ " " ++ show y) False = undefined
   -- eventHandler (Game.EventKey (Game.Char key) kState _ _) game | trace ("eventHandler " ++ show key ++ " " ++ show kState) False = undefined
   eventHandler (Game.EventMotion pos) game@(Game _ _ (Controls _ _ _ _ _ _ _ _ _ _ lmb _ _ _) _) = case lmb of True -> game { controls = (controls game) { pressedCurrPos = pos }}
                                                                                                                False -> game { controls = (controls game) { unpressedLastPos = pos }}
   eventHandler (Game.EventKey (Game.MouseButton Game.LeftButton) kState _ _) game = case kState of Game.Down -> game { controls = (controls game) { lmb = True
                                                                                                                                                   , pressedStartPos = unpressed
                                                                                                                                                   , pressedCurrPos = unpressed }}
                                                                                                                                                   where unpressed = unpressedLastPos $ controls game
                                                                                                    _ -> game { controls = (controls game) { lmb = False}}
   eventHandler (Game.EventKey (Game.SpecialKey Game.KeyLeft) kState _ _) game = case kState of Game.Down -> game { controls = (controls game) { rotl = True, rotr = False }}
                                                                                                _ -> game { controls = (controls game) { rotl = False }}
   eventHandler (Game.EventKey (Game.SpecialKey Game.KeyRight) kState _ _) game = case kState of Game.Down -> game { controls = (controls game) { rotr = True, rotl = False }}
                                                                                                 _ -> game { controls = (controls game) { rotr = False }}
   eventHandler (Game.EventKey (Game.SpecialKey Game.KeyUp) kState _ _) game = case kState of Game.Down -> game { controls = (controls game) { rotu = True, rotd = False }}
                                                                                              _ -> game { controls = (controls game) { rotu = False }}
   eventHandler (Game.EventKey (Game.SpecialKey Game.KeyDown) kState _ _) game = case kState of Game.Down -> game { controls = (controls game) { rotd = True, rotu = False }}
                                                                                                _ -> game { controls = (controls game) { rotd = False }}
   eventHandler (Game.EventKey (Game.Char key) kState _ _) game = case (key, kState) of ('w', Game.Down) -> game { controls = (controls game) { forward = True, backward = False }}
                                                                                        ('w', Game.Up) -> game { controls = (controls game) { forward = False }}
                                                                                        ('s', Game.Down) -> game { controls = (controls game) { backward = True, forward = False }}
                                                                                        ('s', Game.Up) -> game { controls = (controls game) { backward = False }}
                                                                                        ('a', Game.Down) -> game { controls = (controls game) { left = True, right = False }}
                                                                                        ('a', Game.Up) -> game { controls = (controls game) { left = False }}
                                                                                        ('d', Game.Down) -> game { controls = (controls game) { right = True, left = False }}
                                                                                        ('d', Game.Up) -> game { controls = (controls game) { right = False }}
                                                                                        ('i', Game.Down) -> game { controls = (controls game) { up = True, down = False }}
                                                                                        ('i', Game.Up) -> game { controls = (controls game) { up = False }}
                                                                                        ('k', Game.Down) -> game { controls = (controls game) { down = True, up = False }}
                                                                                        ('k', Game.Up) -> game { controls = (controls game) { down = False }}
                                                                                        (_, _) -> game
   eventHandler _ game = game
   
   stepper :: Float -> Game -> Game
   -- stepper time (Game _ (Model _ mtrx) _ _ _ _ ctrls) | trace ("stepper " ++ show mtrx ++ " " ++ show ctrls) False = undefined
   stepper time game = calculateCamera time $ calculateEnvironment time $ calculatePlayer1 time game where
      calculatePlayer1 :: Float -> Game -> Game
      --calculatePlayer1 _ Game {controls = ctrl} | trace ("calculatePlayer1: " ++ show (rotu ctrl)) False = undefined
      calculatePlayer1 time (Game _ _ (Controls False False False False False False False False False False _ _ _ _) _) = game -- just for now
      calculatePlayer1 time game@(Game _ player1@(Model _ _ rotation (Matrix a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44)) controls _) = 
         let tz :: Float
             tz = case controls of Controls {forward = True} -> time
                                   Controls {backward = True} -> (-time)
                                   _ -> 0
             ty :: Float
             ty = case controls of Controls {up = True} -> time
                                   Controls {down = True} -> (-time)
                                   _ -> 0
             tx :: Float
             tx = case controls of Controls {right = True} -> time
                                   Controls {left = True} -> (-time)
                                   _ -> 0
             troth :: Float
             troth = case controls of Controls {rotl = True} -> time
                                      Controls {rotr = True} -> (-time)
                                      _ -> 0
             trotv :: Float
             trotv = case controls of Controls {rotu = True} -> time
                                      Controls {rotd = True} -> (-time)
                                      _ -> 0
             newRotH = rotationQuaternion (Vector3 0 1 0) troth
             newRotV = rotationQuaternion (Vector3 1 0 0) trotv
         in game { player1 = player1 { rotateModel = newRotV `quaternionMult` newRotH `quaternionMult` rotation, translateModel = Matrix a11 a12 a13 (tx * 50 + a14)
                                                                                                                                         a21 a22 a23 (ty * 50 + a24)
                                                                                                                                         a31 a32 a33 (tz * 50 + a34)
                                                                                                                                         a41 a42 a43 a44 }}

      calculateEnvironment :: Float -> Game -> Game
      calculateEnvironment _ game = game
      calculateCamera :: Float -> Game -> Game
      calculateCamera _ game@(Game _ _ (Controls _ _ _ _ _ _ _ _ _ _ _ _ startPos@(x1, y1) currPos@(x2, y2)) (Camera qx qy _ _)) -- change it to checking lmb!
                                                                                 | startPos == currPos = game
                                                                                 | otherwise = game { controls = (controls game) { pressedStartPos = pressedCurrPos $ controls game }
                                                                                                    , camera1 = (camera1 game) { rotateCameraX = quaternionMult rotX (rotateCameraX $ camera1 game)
                                                                                                                             , rotateCameraY = quaternionMult rotY (rotateCameraY $ camera1 game) }}
                                                                                where rotX = rotationQuaternion (Vector3 0 1 0) ((x1 - x2)/2000*pi)
                                                                                      rotY = rotationQuaternion (Vector3 1 0 0) ((y2 - y1)/2000*pi)
