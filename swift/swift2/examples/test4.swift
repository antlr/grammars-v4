import SpriteKit

class GameScene: SKScene {
    var bird = SKSpriteNode()

    var x = !true

    var skyColor = SKColor()
    var verticalPipeGap = 150.0 * 1
    var pipeTextureUp = SKTexture()
    var pipeTextureDown = SKTexture()
    var movePipesAndRemove = SKAction()

    override func didMoveToView(view: SKView) {
        // setup physics
        self.physicsWorld.gravity = CGVectorMake( 0.0, -5.0 )

        // setup background color
        skyColor = SKColor(red: 81.0/255.0, green: 192.0/255.0, blue: 201.0/255.0, alpha: 1.0)
        self.backgroundColor = skyColor

        // ground
        var groundTexture = SKTexture(imageNamed: "land")
        groundTexture.filteringMode = SKTextureFilteringMode.Nearest

        var resetGroundSprite = SKAction.moveByX(groundTexture.size().width * 2.0, y: 0)
        var moveGroundSpritesForever = SKAction.repeatActionForever(SKAction.sequence([moveGroundSprite,resetGroundSprite]))

        for var i:CGFloat = 0; i < 2.0 + self.frame.size.width / ( groundTexture.size().width * 2.0 ); ++i {
            var sprite = SKSpriteNode(texture: groundTexture)
            sprite.setScale(2.0)
            sprite.position = CGPointMake(i * sprite.size.width, sprite.size.height / 2.0)
            sprite.runAction(moveGroundSpritesForever)
            self.addChild(sprite)
        }

        // skyline
        var skyTexture = SKTexture(imageNamed: "sky")
        skyTexture.filteringMode = SKTextureFilteringMode.Nearest

        var moveSkySprite = SKAction.moveByX(-skyTexture.size().width * 2.0, y: 0, duration: NSTimeInterval(0.1 * skyTexture.size().width * 2.0))
        var resetSkySprite = SKAction.moveByX(skyTexture.size().width * 2.0, y: 0, duration: 0.0)
        var moveSkySpritesForever = SKAction.repeatActionForever(SKAction.sequence([moveSkySprite,resetSkySprite]))

        for var i:CGFloat = 0; i < 2.0 + self.frame.size.width / ( skyTexture.size().width * 2.0 ); ++i {
            var sprite = SKSpriteNode(texture: skyTexture)
            sprite.setScale(2.0)
            sprite.zPosition = -20;
            sprite.position = CGPointMake(i * sprite.size.width, sprite.size.height / 2.0 + groundTexture.size().height * 2.0)
            sprite.runAction(moveSkySpritesForever)
            self.addChild(sprite)
        }

        // create the pipes textures
        pipeTextureUp = SKTexture(imageNamed: "PipeUp")
        pipeTextureUp.filteringMode = SKTextureFilteringMode.Nearest
        pipeTextureDown = SKTexture(imageNamed: "PipeDown")
        pipeTextureDown.filteringMode = SKTextureFilteringMode.Nearest

        // create the pipes movement actions
        var distanceToMove = CGFloat(self.frame.size.width + 2.0 * pipeTextureUp.size().width);
        var movePipes = SKAction.moveByX(-distanceToMove, y:0.0, duration:NSTimeInterval(0.01 * distanceToMove));
        var removePipes = SKAction.removeFromParent();
        movePipesAndRemove = SKAction.sequence([movePipes, removePipes]);

        // spawn the pipes
        var spawn = SKAction.runBlock({() in self.spawnPipes()})
        var delay = SKAction.waitForDuration(NSTimeInterval(2.0))
        var spawnThenDelay = SKAction.sequence([spawn, delay])
        var spawnThenDelayForever = SKAction.repeatActionForever(spawnThenDelay)
        self.runAction(spawnThenDelayForever)

        // setup our bird
        var birdTexture1 = SKTexture(imageNamed: "bird-01")
        birdTexture1.filteringMode = SKTextureFilteringMode.Nearest
        var birdTexture2 = SKTexture(imageNamed: "bird-02")
        birdTexture2.filteringMode = SKTextureFilteringMode.Nearest

        var anim = SKAction.animateWithTextures([birdTexture1, birdTexture2], timePerFrame: 0.2)
        var flap = SKAction.repeatActionForever(anim)

        bird = SKSpriteNode(texture: birdTexture1)
        bird.setScale(2.0)
        bird.position = CGPoint(x: self.frame.size.width * 0.35, y:self.frame.size.height * 0.6)
        bird.runAction(flap)


        bird.physicsBody = SKPhysicsBody(circleOfRadius: bird.size.height / 2.0)

        self.addChild(bird)

        // create the ground
        var ground = SKNode()
        ground.position = CGPointMake(0, groundTexture.size().height)
        ground.physicsBody = SKPhysicsBody(rectangleOfSize: CGSizeMake(self.frame.size.width, groundTexture.size().height * 2.0))
        self.addChild(ground)

    }

    func spawnPipes() {
        var pipePair = SKNode()
        pipePair.position = CGPointMake( self.frame.size.width + pipeTextureUp.size().width * 2, 0 );
        pipePair.zPosition = -10;

        var height = UInt32( self.frame.size.height / 4 )
        var y = arc4random() % height + height;

        var pipeDown = SKSpriteNode(texture: pipeTextureDown)
        pipeDown.setScale(2.0)
        pipeDown.position = CGPointMake(0.0, CGFloat(y) + pipeDown.size.height + CGFloat(verticalPipeGap))


        pipeDown.physicsBody = SKPhysicsBody(rectangleOfSize: pipeDown.size)
        pipePair.addChild(pipeDown)

        var pipeUp = SKSpriteNode(texture: pipeTextureUp)
        pipeUp.setScale(2.0)
        pipeUp.position = CGPointMake(0.0, CGFloat(y))

        pipeUp.physicsBody = SKPhysicsBody(rectangleOfSize: pipeUp.size)
        pipePair.addChild(pipeUp)

        pipePair.runAction(movePipesAndRemove);
        self.addChild(pipePair)
    }

    override func touchesBegan(touches: NSSet, withEvent event: UIEvent) {
        /* Called when a touch begins */

        for touch: AnyObject in touches {
            let location = touch.locationInNode(self)

        }
    }

    func clamp(min: CGFloat, max: CGFloat, value: CGFloat) -> CGFloat {
        if( value > max ) {
            return max;
        } else if( value < min ) {
            return min;
        } else {
            return value;
        }
    }


    override func update(currentTime: CFTimeInterval) {
        /* Called before each frame is rendered */
    }
}