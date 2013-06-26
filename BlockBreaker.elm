import Keyboard
import Window

settings = {
    -- 1 unit := radius/1000
    margin=40, -- in pixels
    ballSize=30.0, -- in units
    padWidth= pi/8.0, -- radians
    padHeight=50.0, -- in units
    scoreColor=rgb 0 200 0,
    scoreSize=1,
    blockColor=rgb 200 200 200,
    padSpeed=0.03,
    initialBallPadDistance=30,
    ballSpeed=9,
    fps=50,
    blockSize=60
    }

primitives = {
    block s = square (s (2.0 * settings.blockSize / sqrt 2.0 )) |> filled red,
    pad ph pw color = (rect ph  pw) |> filled color,
    ball s color = (circle (s settings.ballSize)) |> filled color
    }

model = {
    d=2000.0, -- diameter
    r=1000.0,  -- radius
    l= (2 * pi * 1000)
    }

context = {
    diameter=0,
    w=0,
    h=0,
    blocks=[{x=0-120, y=0},
            {x=0, y=120},
            {x=120, y=0},
            {x=0, y=0-120},
            {x=0, y=0-300},
            {x=0, y=300},
            {x=300, y=0},
            {x=0-300, y=0},
            {x=210, y=210},
            {x=0-210, y=210},
            {x=210, y=0-210},
            {x=0-210, y=0-210}],
    players=[{ color=(rgba 200 0 0 0.8),
               angle=0,
               score=0,
               x=1000,
               y=0,
               name="Kazet",
               balls=[{x=0, y=0, vx=1, vy=0, moving=False }]},
             { color=(rgba 0 200 0 0.8),
               angle=180,
               score=0,
               x=0-1000,
               y=0,
               name="Kwaps",
               balls=[{x=100, y=100, vx=0, vy=1, moving=False }]}],
    score = 0 -- score of all players
    }

clearGrey = rgb 100 100 200

flatten listList =
    foldl (++) [] listList

resetBall ball =
    { ball | moving <- False }

resetPlayerBalls player =
    { player | balls <- map (\b -> resetBall b) player.balls }

resetAllBalls context =
    { context | players <- map (\p -> resetPlayerBalls p) context.players }

startBall ball =
    { ball | moving <- True }

startBalls balls =
    map startBall balls

nth lst position = last (take position lst)

movePlayer (player, d) =
    let newAngle = player.angle + d.x * settings.padSpeed
        playerPos angle = (cos angle, sin angle)
        (newX, newY) = playerPos newAngle
        startBallsIfNeeded balls = (if d.y == 1 then startBalls balls else balls)
    in { player | angle <- newAngle,
                  x <- newX * model.r,
                  y <- newY *  model.r,
                  balls <- startBallsIfNeeded player.balls }

movePlayers controls context =
    if length context.players <= length controls then
        { context | players <- map movePlayer (zip context.players controls) }
    else
        error ">2 players are not supported"

near a b distance = abs (a - b) <= distance
nearAngle a b distance =
    let norm angle = atan2 (sin angle) (cos angle)
        na = norm a
        nb = norm b
    in near na nb distance || (near ((norm na) + 2*pi) nb distance) || (near na ((norm nb) + 2*pi)  distance)

within ball player =
    nearAngle (atan2 ball.y ball.x) player.angle ((settings.padWidth / 2.0) + ((settings.ballSize/model.l)* pi)) &&
    (near (sqrt (ball.x * ball.x  + ball.y * ball.y)) 1000.0 (settings.padHeight/2.0))

outOfBoard ball = (near (sqrt (ball.x * ball.x  + ball.y * ball.y)) 1050.0 (settings.padHeight/2.0))

detectCollision p ball =
    let
        b_angle = 2 * (p.angle + pi) - atan2 (0- ball.vy) (0 - ball.vx)
        revertedBall b = {b | vy <- sin b_angle,
                              vx <- cos b_angle
                              }
    in if | within ball p -> revertedBall ball
          | outOfBoard ball -> resetBall ball
          | otherwise -> ball

detectPlayerCollisions player context =
    { player | balls <- map (\ball -> detectCollision player ball) player.balls }

dist a b = sqrt $ (a.x - b.x)^2 + (a.y - b.y)^2

distCity a b = abs (a.x - b.x) + abs (a.y - b.y)

sgn x = if | x == 0 -> 0
           | x < 0  -> 0-1
           | otherwise -> 1

ballBlockCollision block (ball, blocks)=
    let
        collides = distCity ball block <= (settings.ballSize + settings.blockSize)
        block_angle = let s = (sgn (block.x - ball.x), sgn (ball.y - block.y)) in
        if | s == (0-1, 0-1) -> pi/4
           | s == (0-1, 1) -> 3.0 * pi/4
           | s == (1, 1) -> 5.0 * pi/4
           | s == (1, 0-1) -> 7.0 * pi/4
           | s == (0, 0-1) -> 0.0
           | s == (0, 1) -> pi/2.0
           | s == (0-1, 0) -> pi/2.0
           | otherwise  -> 3.0 *  pi/2.0
        ball_angle = pi +  (2 * (block_angle) - atan2 (0- ball.vy) (0 - ball.vx))
        newBall = { ball | vx <- cos ball_angle,
                           vy <- sin ball_angle }
    in if collides then (newBall, blocks) else (ball, block::blocks)

detectBallCollisions ball (context, balls) =
    let (newBall, newBlocks)  = foldl ballBlockCollision (ball, []) context.blocks
    in ({ context | blocks <- newBlocks }, newBall::balls)

detectPlayerBallCollisions player (context, players) =
    let (newContext, newBalls)  = foldl detectBallCollisions (context, []) player.balls
    in (newContext, players ++ [{player | balls <- newBalls}])

detectCollisions context =
    let ctxAfterPlayerCollisions = { context | players <- map (\p -> detectPlayerCollisions p context) context.players }
        (ctxAfterBallCollisions, newPlayers) = foldl detectPlayerBallCollisions (ctxAfterPlayerCollisions, []) ctxAfterPlayerCollisions.players
    in { ctxAfterBallCollisions | players <- newPlayers }

moveBall ball time player context =
    if ball.moving then
        { ball | x <- ball.x + ball.vx * settings.ballSpeed * time,
                 y <- ball.y + ball.vy * settings.ballSpeed * time }
    else { ball | x <- (model.r - settings.initialBallPadDistance) * cos player.angle,
                  y <- (model.r - settings.initialBallPadDistance) * sin player.angle,
                  vx <- 0-cos player.angle,
                  vy <- 0-sin player.angle }

moveBalls time context =
    let mover player = { player | balls <- map (\ball -> moveBall ball time player context) player.balls }
    in {context | players <- map mover context.players }

updateDimensions (width, height) context =
    let d = (min width height) - 2 * settings.margin
    in { context | radius <- d / 2,
                   diameter <- d,
                   w <- width,
                   h <- height,
                   factor <- d/2000 }

step (delta, directions, dimensions) =
    updateDimensions dimensions .
    movePlayers directions .
    moveBalls delta .
    detectCollisions

txt f = text . f . monospace . Text.color settings.scoreColor . toText

render context =
    let s = (*) context.factor in
        let drawBall ball color = (primitives.ball s color) |> move (s ball.x, s ball.y)
            drawPlayer player =
                let
                    pw = (sin (settings.padWidth / 2.0) * 2 * context.radius)
                    ph = (s settings.padHeight)
                    balls = map (\ball -> drawBall ball player.color) player.balls
                in  balls ++ [(primitives.pad ph pw player.color) |> move (s player.x, s player.y) |> rotate player.angle]

            drawBlock block = (primitives.block s) |> move (s block.x, s block.y) |> rotate (pi/4)
            makeScores player = " " ++ player.name ++ show player.score
            scoreText = txt (Text.height settings.scoreSize) (foldr (++) " " (map makeScores context.players))
            scores = [toForm scoreText |> move (0, context.radius + settings.margin / 2)]
            players = flatten (map drawPlayer context.players)
            blocks = map drawBlock context.blocks
            board = [filled clearGrey (circle context.radius)]
            gameOver = if length context.blocks == 0 then [(txt (Text.height (5 * settings.scoreSize)) "GAME OVER") |> toForm |> move (s 200, s 400)] else []
    in collage (context.diameter + 2 * settings.margin) (context.diameter + 2 * settings.margin) (board ++ players ++ scores ++ blocks ++ gameOver)


input =
    let delta = lift (\t -> t/20) (fps settings.fps)
        toSignal delta keyboard_wasd keyboard_arrows window_dimensions = (delta, [keyboard_wasd, keyboard_arrows], window_dimensions)
    in sampleOn delta (lift4 toSignal delta Keyboard.wasd Keyboard.arrows Window.dimensions)

main  = lift render (foldp step (resetAllBalls context) input)
