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
    padSpeed=0.1,
    ballSpeed=6,
    fps=50,
    blockSize=60
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
    players=[{color=(rgba 200 0 0 60), angle=0, score=0, x=1000, y=0, name="Kazet"},
             {color=(rgba 0 200 0 60), angle=180, score=0, x=0-1000, y=0, name="Kwaps"}],
    balls=[{x=0, y=0, vx=1, vy=0},
           {x=100, y=100, vx=0, vy=1}],
    blocks=[{x=0-120, y=0}, {x=0, y=120}, {x=220, y=150} ],
    score = 0 -- score of all players
    }



nth lst position = last (take position lst)

clearGrey = rgb 100 100 200

movePlayer (player, {x}) =
    let newAngle = player.angle + x * settings.padSpeed
        playerPos angle = (cos angle, sin angle)
        (newX, newY) = playerPos newAngle
    in { player | angle <- newAngle, x <- newX * model.r , y <- newY *  model.r }

movePlayers controls context =
    let mover playerControlTuple = movePlayer playerControlTuple
    in if length context.players <= length controls then
        { context | players <- map mover (zip context.players controls) }
    else
        error ">2 players are not supported"

near a b distance = abs (a - b) <= distance
nearAngle a b distance =
    let norm angle = atan2 (sin angle) (cos angle)
        na = norm a
        nb = norm b
     in near na nb distance || (near ((norm na) + 2*pi) nb distance) || (near na ((norm nb) + 2*pi)  distance)

within ball player = nearAngle (atan2 ball.y ball.x) player.angle ((settings.padWidth / 2.0)+ ((settings.ballSize/model.l)* pi)) &&
       (near (sqrt (ball.x * ball.x  + ball.y * ball.y)) 1000.0 (settings.padHeight/2.0))

detectCollision p ball =
    let
        b_angle = 2 * (p.angle + pi) - atan2 (0- ball.vy) (0 - ball.vx)
        revertedBall b = {b | vy <- sin b_angle,
                              vx <- cos b_angle
                              }
    in if within ball p then revertedBall ball else ball

detectPlayerCollisions player context =
    let detector balls = detectCollision player balls
    in {context | balls <- map detector context.balls }


dist a b = sqrt $ (a.x - b.x) ^2 + (a.y - b.y)^2

detectBallCollisions ball context =
    let
        collides block = dist ball block <= (settings.ballSize + settings.blockSize)
        colliding = filter collides context.blocks
        notColliding = filter (not . collides) context.blocks
    in
        {context | blocks <- notColliding, score <- context.score + (length colliding) }



detectCollisions context =
    let newContext = foldl detectPlayerCollisions context context.players
    in foldl detectBallCollisions newContext newContext.balls

moveBall ball time = { ball | x <- ball.x + ball.vx * settings.ballSpeed * time,
                              y <- ball.y + ball.vy * settings.ballSpeed * time }

moveBalls time context =
    let mover ball = moveBall ball time
    in {context | balls <- map mover context.balls }

updateDimensions (width, height) context =
    let d = (min width height) - 2 * settings.margin
    in
        { context | radius <- d / 2,
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
        let drawBall ball = (circle (s settings.ballSize) |> filled white |> move (s ball.x, s ball.y))
            drawPad player =
                let
                    pw = (sin (settings.padWidth / 2.0) * 2 * context.radius)
                    ph = (s settings.padHeight)
                in (rect ph  pw) |> filled player.color |> move (s player.x, s player.y) |> rotate player.angle

            drawBlock block = circle (s settings.blockSize) |> filled red |> move (s block.x, s block.y)
            makeScores player = " " ++ player.name ++ show context.score
            scoreText = txt (Text.height settings.scoreSize) (foldr (++) " " (map makeScores context.players))
            scores = [toForm scoreText |> move (0, context.radius + settings.margin / 2)]
            pads = map drawPad context.players
            balls = map drawBall context.balls
            blocks = map drawBlock context.blocks
            board = [filled clearGrey (circle context.radius)]
        in collage (context.diameter + 2 * settings.margin) (context.diameter + 2 * settings.margin) (board ++ pads ++ balls ++ scores ++ blocks)


input = let delta = lift (\t -> t/20) (fps settings.fps)
            toSignal delta keyboard_wasd keyboard_arrows window_dimensions = (delta, [keyboard_wasd, keyboard_arrows], window_dimensions)
        in sampleOn delta (lift4 toSignal delta Keyboard.wasd Keyboard.arrows Window.dimensions)

main  = lift render (foldp step context input)
