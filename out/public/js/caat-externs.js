CAAT.Director = function() {};
CAAT.Director.prototype.addAudio = function() {};
CAAT.Director.prototype.createScene = function() {};
CAAT.Director.prototype.getImage = function() {};
CAAT.Director.prototype.getAudioManager = function() {};
CAAT.Director.prototype.audioPlay = function() {};
CAAT.Director.prototype.audioLoop = function() {};
CAAT.Director.prototype.ctx = function() {};
CAAT.Director.prototype.width = function() {};
CAAT.Director.prototype.height = function() {};

CAAT.Scene = function() {};
CAAT.Scene.prototype.addChild = function() {};
CAAT.Scene.prototype.addChildImmediately = function() {};
CAAT.Scene.prototype.addChildDelayed = function() {};
CAAT.Scene.prototype.createTimer = function() {};

CAAT.AudioManager = function() {};
CAAT.AudioManager.prototype = function() {};
CAAT.AudioManager.prototype.getAudio = function() {};

CAAT.Behavior = function() {};
CAAT.Behavior.prototype.setFrameTime = function() {};
CAAT.Behavior.prototype.setValues = function() {};
CAAT.Behavior.prototype.setPingPong = function() {};
CAAT.Behavior.prototype.setCycle = function() {};
CAAT.Behavior.prototype.addListener = function() {};
CAAT.Behavior.prototype.getDuration = function() {};
CAAT.Behavior.prototype.getStartTime = function() {};

CAAT.ContainerBehavior = function() {};
CAAT.ContainerBehavior.prototype.addBehavior = function() {};

CAAT.RotateBehavior = function() {};

CAAT.PathBehavior = function() {};
CAAT.PathBehavior.prototype.setAutoRotate = function() {};
CAAT.PathBehavior.prototype.setPath = function() {};
CAAT.Path = function() {};
CAAT.Path.prototype.setLinear = function() {};
CAAT.Path.prototype.beginPath = function() {};
CAAT.Path.prototype.endPath = function() {};
CAAT.Path.prototype.addQuadricTo = function() {};
CAAT.ScaleBehavior = function() {};

CAAT.ActorContainer = function() {};
CAAT.ActorContainer.prototype.addChild = function() {};
CAAT.ActorContainer.prototype.getChildAt = function() {};
CAAT.ActorContainer.prototype.setZOrder = function() {};

CAAT.Actor = function() {};
CAAT.Actor.prototype.setLocation = function() {};
CAAT.Actor.prototype.setScale = function() {};
CAAT.Actor.prototype.setRotation = function() {};
CAAT.Actor.prototype.setBackgroundImage = function() {};
CAAT.Actor.prototype.addBehavior = function() {};
CAAT.Actor.prototype.centerAt = function() {};
CAAT.Actor.prototype.setFillStyle = function() {};
CAAT.Actor.prototype.setSpriteIndex = function() {};
CAAT.Actor.prototype.enableDrag = function() {};
CAAT.Actor.prototype.enableEvents = function() {};
CAAT.Actor.prototype.paint = function() {};
CAAT.Actor.prototype.mouseEnter = function() {};
CAAT.Actor.prototype.mouseExit = function() {};
CAAT.Actor.prototype.mouseUp = function() {};
CAAT.Actor.prototype.mouseDown = function() {};
CAAT.Actor.prototype.mouseClick = function() {};
CAAT.Actor.prototype.mouseDrag = function() {};
CAAT.Actor.prototype.setOutline = function() {};
CAAT.Actor.prototype.setOutlineColor = function() {};
CAAT.Actor.prototype.setExpired = function() {};
CAAT.Actor.prototype.setDiscardable = function() {};
CAAT.Actor.prototype.cacheAsBitmap = function() {};
CAAT.Actor.prototype.modelToModel = function() {};
CAAT.Actor.prototype.modelToView = function() {};
CAAT.Actor.prototype.x = function() {};
CAAT.Actor.prototype.y = function() {};
CAAT.Actor.prototype.time = function() {};
CAAT.Actor.prototype.emptyBehaviorList = function() {};
CAAT.Actor.prototype.setBounds = function() {};
CAAT.Actor.prototype.setSize = function() {};
CAAT.Actor.prototype.setAlpha = function() {};

CAAT.MouseEvent = function() {};
CAAT.MouseEvent.prototype.point = function() {};
CAAT.MouseEvent.prototype.screenPoint = function() {};
CAAT.MouseEvent.prototype.time = function() {};
CAAT.MouseEvent.prototype.x = function() {};
CAAT.MouseEvent.prototype.y = function() {};

CAAT.Font = function() {};
CAAT.TextActor = function() {};
CAAT.TextActor.prototype.setFont = function() {};
CAAT.TextActor.prototype.setText = function() {};
CAAT.TextActor.prototype.calcTextSize = function() {};
CAAT.TextActor.prototype.textWidth = function() {};
CAAT.TextActor.prototype.textHeight = function() {};
CAAT.TextActor.prototype.create = function() {};

CAAT.ShapeActor = function() {};
CAAT.ShapeActor.prototype.SHAPE_RECTANGLE = function() {};
CAAT.ShapeActor.prototype.SHAPE_CIRCLE = function() {};
CAAT.ShapeActor.prototype.setShape = function() {};

CAAT.Point = function() {};

CAAT.Image = function() {};
CAAT.Image.prototype.initialize = function() {};

CAAT.SpriteImage = function() {};
CAAT.SpriteImage.prototype.setAnimationImageIndex = function() {};
CAAT.SpriteImage.prototype.setChangeFPS = function() {};
CAAT.SpriteImage.prototype.setSpriteIndex = function() {};
CAAT.SpriteImage.prototype.spriteIndex = function() {};

CAAT.AlphaBehavior = function() {};

CAAT.modules.ImageUtil.createThumb = function() {};
CAAT.modules.initialization.init = function() {};


