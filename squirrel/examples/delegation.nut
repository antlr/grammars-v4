
PEntity <- {
    name="noname"
    pos={x=0,y=0,z=0}
    type="entity"
    //methamethod
    _typeof=function()
    {
        return type;
    }
}

function PEntity::PrintPos()
{
    ::print("x="+pos.x+" y="+pos.y+" z="+pos.z+"\n");
}

function PEntity::new(name,pos)
{
    local newentity=clone ::PEntity;
    if(name)
        newentity.name=name;
    if(pos)
        newentity.pos=pos;
    return newentity;
}

PPlayer <- {
    model="warrior.mdl"
    weapon="fist"
    health=100
    armor=0
    //overrides the parent type
    type="player"
}

function PPlayer::new(name,pos)
{
    local p = clone ::PPlayer;
    local newplayer = ::PEntity.new(name,pos);
    newplayer.setdelegate(p);
    return newplayer;
}

local player=PPlayer.new("godzilla",{x=10,y=20,z=30});

::print("PLAYER NAME"+player.name+"\n");
::print("ENTITY TYPE"+typeof player+"\n");

player.PrintPos();

player.pos.x=123;

player.PrintPos();
