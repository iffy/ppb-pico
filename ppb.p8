pico-8 cartridge // http://www.pico-8.com
version 10
__lua__
-- proud pink balloon
-- by matt haggard

center=0
right=1
left=2

black=0
dark_blue=1
dark_purple=2
dark_green=3
brown=4
dark_gray=5
light_gray=6
white=7
red=8
orange=9
yellow=10
green=11
blue=12
indigo=13
pink=14
peach=15

solid_flag = 1
ladder_flag = 2

function log(...)
    local args = {...}
    local s = ''
    for i=1,#args do
        s = s..repr(args[i])..' '
    end
    printh(s, "pico.log")
end

function repr(x)
    if (type(x) == 'boolean') then
        if (x) return 'true'
        return 'false'
    elseif (type(x) == 'table') then
        return '<table>'
    else
        return ''..x
    end
end

function ceil(x)
    return flr(x)+1
end

--
-- http://stackoverflow.com/questions/2282444/how-to-check-if-a-table-contains-an-element-in-lua
--
function addtoset(set, key)
    set[key] = true
end
function removefromset(set, key)
    set[key] = nil
end
function setcontains(set, key)
    return set[key] ~= nil
end

--
-- a limited deferred.
--
d_id = 1
function deferred()
    local d = {}
    d.id = d_id
    d_id += 1
    d.has_result = false
    d.cbi = 1
    d.callbacks = {}
    d.than = function(cb)
        add(d.callbacks, cb)
        d.pump()
        return d
    end
    d.pump = function()
        if (not(d.has_result) or (type(d.result) == 'table' and d.result.than)) return
        for i=d.cbi,#d.callbacks do
            d.result = d.callbacks[i](d.result)
            d.cbi += 1
            if (type(d.result) == 'table' and d.result.than) then
                d.result.than(d.resolve)
                break
            end    
        end
    end
    d.resolve = function(result)
        d.has_result = true
        d.result = result
        d.pump()
    end
    return d
end
function unpack(t, from, to)
  from = from or 1
  to = to or #t
  if from > to then return end
  return t[from], unpack(t, from+1, to)
end
-- ignore a deferred result, then run another function
function thencall(d, func, ...)
    local args = {...}
    d.than(function(ignored)
        return func(unpack(args))
    end)
    return d
end


-- all the actors in the world
actors = {}
tethers = {}

nextid = 1
-- largely copied from collide.p8
-- make an actor
-- x,y means center of the actor in map tiles (not pixels)
function make_actor(tx, ty, type)
    a = {}
    a.id = ''..nextid
    nextid += 1
    -- posish
    a.pos = vector(tx, ty)
    -- veloc
    a.vel = vector(0, 0)
    -- accel
    a.accel = vector(0, 0)
    -- hitbox
    a.w = 0.31
    a.h = 0.31
    a.type = type
    a.collides_with = {}
    a.beholden_to_walls = true
    a.on_collide = function(other) end
    a.standing = false
    a.climber = false

    a.anchorx = 0
    a.anchory = 0

    a.facing = right

    a.frame = 0
    a.frames = 1
    a.frameticks = 4

    a.inertia = 0.6
    a.air_inertia = 0.6
    a.land_inertia = 0.6
    a.bounce = 0.4
    a.mass = 1
    a.sprite = 1
    -- number of "walking frames"
    add(actors, a)
    return a
end
function destroy_actor(a)
    del(actors, a)
end
function destroy_all_actors()
    actors = {}
end

--------------------------------------------------
-- balloon
--------------------------------------------------
function draw_balloon(px, py, balloon)
    -- balloon
    pal(pink, balloon.color)
    spr(1, px, py)
    pal()
    
    -- string
    if (balloon.drawstring) then
        offset = 0
        if (balloon.string_facing == right) then
            offset = 2
        elseif (balloon.string_facing == left) then
            offset = 1
        end
        spr(2+offset, px, py+8)
    end
end
function make_balloon(tx, ty)
    balloon = make_actor(tx, ty, 'balloon')
    balloon.draw = draw_balloon
    balloon.drawstring = false
    balloon.accel.y = -0.02
    balloon.w = 0.3
    balloon.h = 0.3
    addtoset(balloon.collides_with, 'girl')
    addtoset(balloon.collides_with, 'balloon')
    balloon.air_inertia = 0.9
    balloon.land_inertia = 0.6
    balloon.mass = 0.1
    balloon.string_lag = 3
    balloon.string_facing = center
    balloon.control = nil
    balloon.anchory = 4
    balloon.color = pink
    return balloon
end
function control_balloon(balloon)
    accel = 0.04
    local going = vector(0,0)
    if (btn(0)) going.x -= 1 balloon.vel.x -= accel balloon.string_lag = 3 balloon.string_facing = left
    if (btn(1)) going.x += 1 balloon.vel.x += accel balloon.string_lag = 3 balloon.string_facing = right
    if (btn(2)) going.y -= 1 balloon.vel.y -= accel
    if (btn(3)) going.y += 1 balloon.vel.y += accel
    if (btnp(4)) then
        balloon.vel = vadd(balloon.vel, vmul(vnorm(going), 0.5))
    end


    if (balloon.string_lag > 0) then
        balloon.string_lag -= 1
    else
        balloon.string_facing = center
    end

end



--------------------------------------------------
-- girl
--------------------------------------------------
gravity = 0.10
function draw_girl(px, py, girl)
    local flip_x = false
    if (girl.facing == left) flip_x = true
    pal(red, girl.shirt_color)
    local frame = 7 + girl.frame
    if not(girl.standing) then
        -- jumping or falling
        frame = 12
        if (abs(girl.vel.x) >= 0.05) then
            -- going sideways fast
            frame = 27
        end
    end
    
    spr(frame, px, py, 1, 1, flip_x)
    pal()
    -- print('v:('..girl.vel.x..','..girl.vel.y..')',
    --     girl.pos.x*8, girl.pos.y*8, white)
    -- if (girl.standing) print('land', girl.pos.x*8, (girl.pos.y+0.5)*8)
end
function make_girl(tx, ty)
    girl = make_actor(tx, ty, 'girl')
    girl.draw = draw_girl
    girl.control = ai_walk_around
    girl.shirt_color = red
    girl.accel.y = gravity
    girl.bounce = 0
    girl.land_inertia = 0.3
    girl.air_inertia = 0.95

    girl.climber = {
        climbing=false,
    }
    return girl
end
function ai_walk_around(girl)
    if (girl.climber.climbing) then
        -- climbing
        girl.accel.y = 0
    else
        -- not climbing
        girl.accel.y = gravity

        -- look for a ladder
        ladder = nearest_ladder(girl)
        if (ladder == 0) then
            -- on a ladder
            girl.shirt_color = orange
            girl.climber.climbing = true
        else
            girl.shirt_color = purple
        end

        -- walk around
        if (girl.standing) then
            local r = rnd(20)
            if (r <= 1) then
                -- change what you're doing
                r = rnd(7)
                if (girl.accel.x != 0) then
                    -- she's moving
                    girl.accel.x = 0
                else
                    -- she's holding still
                    if (r <= 5) then
                        girl.accel.x = rnd(0.3)
                    else
                        girl.accel.x = -rnd(0.3)
                    end
                end
            end
        end

        -- jump
        local r2 = rnd(20)
        if (r2 <= 1 and girl.vel.y == 0) then
            -- jump
            girl.vel.y -= 1
            if (rnd(10) < 5) then
                girl.vel.x += 0.7
            else
                girl.vel.x -= 0.7
            end
            girl.accel.x = 0
        end
    end
end
chase_target = nil
function chase_balloon(girl)
    if (not(chase_target)) then
        ai_walk_around(girl)
    end
end

--------------------------------------------------
-- man
--------------------------------------------------
function draw_man(px, py, man)
    local flip_x = false
    if (man.facing == left) flip_x = true
    --pal(red, girl.shirt_color)
    -- top
    spr(13, px, py-8, 1, 1, flip_x)

    -- bottom
    spr(29+man.frame, px, py, 1, 1, flip_x)
    --pal()
end
function make_man(tx, ty)
    man = make_actor(tx, ty, 'man')
    man.draw = draw_man
    man.control = control_man
    man.anchorx = -3
    man.anchory = -3
    man.beholden_to_walls = false
    return man
end
function control_man(man)
end

--------------------------------------------------
-- bird
--------------------------------------------------
function draw_bird(px, py, bird)
    local flip_x = false
    if (bird.facing == left) flip_x = true
    pal(brown, bird.color)
    spr(71+bird.frame, px, py, 1, 1, flip_x)
    pal()
end
function make_bird(tx, ty)
    bird = make_actor(tx, ty, 'bird')
    bird.draw = draw_bird
    addtoset(bird.collides_with, 'balloon')
    bird.inertia = 1
    bird.air_inertia = 1
    bird.land_inertia = 1
    bird.mass = 0.5
    bird.vel.x = 0.2
    bird.frameticks = 1
    bird.control = control_bird
    bird.color = brown
    bird.beholden_to_walls = false
    return bird
end
function control_bird(bird)
    if (bird.pos.x > 18 or bird.pos.x < -2) destroy_actor(bird)
end


--------------------------------------------------
-- tethers
--------------------------------------------------
function make_tether(obj1, obj2, len)
    t = {}
    t.objs = {obj1, obj2}
    t.elasticity = 0.1
    t.length = len or 2
    t.color = white
    t.breaking_point = 0.2
    t.onbreak = nil
    add(tethers, t)
    return t
end
function destroy_tether(t)
    del(tethers, t)
end
function draw_tether(t)
    o1 = t.objs[1]
    o2 = t.objs[2]
    if (o2) then
        line(o1.pos.x * 8 + o1.anchorx,
             o1.pos.y * 8 + o1.anchory,
             o2.pos.x * 8 + o2.anchorx,
             o2.pos.y * 8 + o2.anchory,
             t.color)
    else
        -- detached tether
        -- for now, just drop it down
        line(o1.pos.x * 8 + o1.anchorx,
             o1.pos.y * 8 + o1.anchory,
             o1.pos.x * 8 + o1.anchorx,
             (o1.pos.y + t.length) * 8 + o1.anchory,
             t.color)
    end
end
function constrain_tether(t)
    obj1 = t.objs[1]
    obj2 = t.objs[2]
    if (obj2) then
        n1 = vadd(obj1.pos, obj1.vel)
        n2 = vadd(obj2.pos, obj2.vel)
        vect = vsub(n2, n1)
        d = vmag(vect)
        over = d - t.length
        if (t.onbreak and over/t.length > t.breaking_point) then
            t.onbreak()
        elseif (over > 0) then
            norm = vnorm(vect)

            obj1.vel = vadd(obj1.vel, vmul(norm, t.elasticity * (1 + (over / t.length))))
            -- uncomment if you want the string to pull both ways
            -- obj2.vx -= vectx * t.elasticity
            -- obj2.vy -= vecty * t.elasticity
        end
    else
        -- detached tether
    end
end
function distance(x1,y1,x2,y2)
    return sqrt((x2-x1)^2 + (y2-y1)^2)
end

--------------------------------------------------
-- generic actor stuff
--------------------------------------------------
function draw_actor(a)
    a.draw(
        (a.pos.x * 8) - 4,
        (a.pos.y * 8) - 4,
        a)
end
function move_actor(a)
    a.should_advance = {x=true,y=true}

    if (a.control) a.control(a)

    -- accelerate
    a.vel = vadd(a.vel, a.accel)

    -- actor:actor collisions
    colliders = collide_actor(a)

    -- actor:wall collisions
    if (a.beholden_to_walls) collide_with_walls(a)

    -- move
    if (a.should_advance.x) a.pos.x += a.vel.x
    if (a.should_advance.y) a.pos.y += a.vel.y

    -- decelerate
    if (a.standing) then
        a.inertia = a.land_inertia
    else
        a.inertia = a.air_inertia
    end
    a.vel = vmul(a.vel, a.inertia)

    a.frame += a.frameticks * (abs(a.vel.x) + abs(a.vel.y))
    a.frame %= (a.frames+1)

    if (a.vel.x < 0) then
        a.facing = left
    elseif (a.vel.x > 0) then
        a.facing = right
    end
end
--
-- vector functions
--
function vector(x, y)
    local v = {}
    v.x = x
    v.y = y
    return v
end
function vadd(a, b)
    return vector(a.x+b.x, a.y+b.y)
end
function vsub(a, b)
    return vector(a.x-b.x, a.y-b.y)
end
function vmul(v, f)
    return vector(v.x*f, v.y*f)
end
function vnorm(a)
    angle = atan2(a.x, a.y)
    return vector(cos(angle), sin(angle))
end
function vmag(a)
    return distance(0,0,a.x,a.y)
end
function vdot(a, b)
    return a.x*b.x + a.y*b.y
end
function vcross(a, b)
    return a.x*b.y - a.y*b.x
end
--
-- find the intersection point of two rays
-- http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
--
function vintersection(a, ar, b, br)
    den = vcross(ar, br)
    unum = vcross(vsub(a, b), ar)
    if (den == 0) then
        if (unum == 0) then
            -- collinear (same line)
            t0 = vdot(vsub(b, a), ar) / vdot(ar, ar)
            s_dot_r = vdot(br, ar)
            t1 = t0 + (s_dot_r) / vdot(ar, ar)
            return false
        else
            -- parallel
            return false
        end
    else
        t = vcross(vsub(b, a), br) / den
        u = unum / den
        return {
            point=vector(vadd(a, vmul(ar, t)), vadd(b, vmul(br, u))),
            apercent=t,
            bpercent=u,
        }
    end
end
--
-- check and react to collisions for a particular actor
-- returns a table of actors which are collided
--
done_collisions = {}
function collide_actor(a)
    -- this actor's future position
    pos = a.pos
    vel = a.vel
    npos = vadd(pos, vel)

    for o in all(actors) do
        pair_key = min(a.id, o.id)..","..max(a.id, o.id)
        if (not(setcontains(done_collisions, pair_key))
            and o != a
            and (
                setcontains(o.collides_with, a.type)
                or setcontains(a.collides_with, o.type))
            ) then
            
            -- o actor's current and future positions
            opos = o.pos
            onpos = vadd(opos, o.vel)

            -- delta (current and future)
            d = vsub(opos, pos)
            nd = vsub(onpos, npos)

            overlapx = a.w+o.w-abs(nd.x)
            overlapy = a.h+o.h-abs(nd.y)

            local deets = {
                cur_distance=d,
                fut_distance=nd,
                bounce=false,
            }
            local collide = false;
            if (overlapx > 0 and overlapy > 0) then
                -- they are touching
                collide = true
                
                -- they are getting closer
                if (vmag(nd) < vmag(d)) deets.bounce=true
            else
                -- are they moving too fast to collide in a single frame?
                isection = vintersection(pos, vel, opos, o.vel)
                if (isection
                    and isection.apercent >= 0
                    and isection.apercent <= 1
                    and isection.bpercent >= 0
                    and isection.bpercent <= 1) then
                    -- they intersected
                    deets.bounce = true
                    collide = true
                end
            end
            if (collide) then
                if (a.type <= o.type) then
                    on_collide(a, o, deets)
                else
                    on_collide(o, a, deets)
                end
            end
        end
        addtoset(done_collisions, pair_key)
    end
end

function on_collide(a, b, deets)
    if (deets.bounce) bounce_actors(a, b, deets)
end

-- bounce two actors that have collided
function bounce_actors(a, b, deets)
    norm = vnorm(deets.cur_distance)

    a_dot = vdot(a.vel, norm)
    b_dot = vdot(b.vel, norm)

    totmass = (a.mass + b.mass)

    optimizedp = (2 * (a_dot - b_dot)) / totmass

    a.vel = vsub(a.vel, vmul(norm, optimizedp * b.mass))
    b.vel = vadd(b.vel, vmul(norm, optimizedp * a.mass))

    sfx(0)
end


-- check if a cell is solid
function is_solid(x, y)
    if (x < 0
        or x > map_w
        or y < 0
        or y > map_h) then
        return true
    end
    return fget(getmaptile(x, y), solid_flag)
end
-- this only works for w,h less than 1
-- it checks the 4 corners
function in_solid(x,y,w,h)
    return is_solid(x-w, y-h) 
        or is_solid(x+w, y-h)
        or is_solid(x-w, y+h)
        or is_solid(x+w, y+h)
end

function snap(a, direction)
    while (not(in_solid(a.pos.x, a.pos.y, a.w, a.h))) do
        a.pos = vadd(a.pos, direction)
    end
end

-- bug: fast-moving balloons can shoot through single-width solid things
-- bug: diagonally-moving balloons can get stuck in walls
bounce_thresh = 0.01
function collide_with_walls(a)
    npos = vadd(a.pos, a.vel)
    a.standing = false

    if (in_solid(npos.x, a.pos.y, a.w, a.h/2)) then
        a.should_advance.x = false
        a.vel.x *= -a.bounce
    end

    if (a.vel.y < 0) then
        -- going up
        if (in_solid(a.pos.x, npos.y, a.w, a.h)) then
            
            if (a.bounce > 0 and abs(a.vel.y) > bounce_thresh) then
                -- bounce off ceiling
                a.should_advance.y = false
                a.vel.y *= -a.bounce
            else
                -- snap to ceiling
                snap(a, vector(0, -0.01))
                a.vel.y = 0
            end
        end
    else
        -- going down
        if (in_solid(a.pos.x, npos.y, a.w, a.h)) then
            if (a.bounce > 0 and abs(a.vel.y) > bounce_thresh) then
                -- bounce off ground
                a.should_advance.y = false
                a.vel.y *= -a.bounce
            else
                -- snap to ground
                snap(a, vector(0, 0.01))
                a.standing = true
                a.vel.y = 0
            end
        end
    end
end

--------------------------------------------------
-- AI bot npc
--------------------------------------------------
-- return true pct% of the time
--   e.g. sometimes(10) will return true 10% of the time
function sometimes(pct)
    return rnd(100) < pct
end
function choose(...)
    local args = {...}
    return args[flr(rnd(#args))+1]
end

running_cooldowns = {}
function start_cooldown(key, seconds)
    addtoset(running_cooldowns, key)
    set_timeout(seconds, function()
        removefromset(running_cooldowns, key)
    end)
end
function cooling_off(key)
    return setcontains(running_cooldowns, key)
end

-- return the direction to the nearest ladder
-- within a few blocks
-- 1 = to the right
-- -1 = to the left
-- 0 = you're on a ladder
function nearest_ladder(a)
    if (fget(getmaptile(a.pos.x, a.pos.y), ladder_flag)) return 0
    for i = 0, 4 do
        if (fget(getmaptile(a.pos.x + i, a.pos.y), ladder_flag)) return 1
        if (fget(getmaptile(a.pos.x - i, a.pos.y), ladder_flag)) return -1
    end
    return nil
end

function make_fsm(handlers)
    local state = 'init'
    local change_state = function(new_state)
        if (not(new_state)) return
        -- run state teardown handler
        local off_handler = handlers['off_'..state]
        if (off_handler) off_handler()
        -- run state init handler
        local on_handler = handlers['on_'..new_state]
        if (on_handler) on_handler()
        state = new_state
    end
    local tick = function()
        if (handlers.pre_tick) change_state(handlers.pre_tick())
        change_state(handlers[state]())
    end
    return tick
end

function fsm_chaser(a)
    return make_fsm{
        pre_tick=function()
        end,

        init=function()
            if (a.standing) return 'idle'
            return 'idle'
        end,

        -- idle
        on_idle=function()
            a.accel.x = 0
            start_cooldown(a, 2)
        end,
        idle=function()
            if (cooling_off(a)) return
            if (a.standing and sometimes(20)) return 'walking'
        end,

        -- walking
        on_walking=function()
            a.accel.x = choose(0.15, -0.15)
            start_cooldown(a, 1)
        end,
        walking=function()
            if (cooling_off(a)) return
            return 'idle'
        end
    }
end



--------------------------------------------------
-- mapping
--------------------------------------------------
map_src_x = 0
map_src_y = 0
map_w = 16
map_h = 16
-- choose a section of the map as the part
-- to draw for collision purposes
function choosemap(map_x, map_y, w, h)
    map_src_x = map_x
    map_src_y = map_y
    map_w = w
    map_h = h
end
-- get a map tile where x,y is a coordinate within
-- the map chosen by choosemap
function getmaptile(x, y)
    return mget(map_src_x+x, map_src_y+y)
end
-- draw the chosen map
function draw_chosen_map()
    map(map_src_x, map_src_y, 0, 0, map_w, map_h)
end
-- move the camera to be looking at a specific actor
function put_camera_on(actor)
    -- the screen is 128 pixels wide, so the
    -- 128 is to prevent from going to far right
    -- 64 is to center the actor
    cam_y = mid(0, actor.pos.y*8-64, map_h*8-128)
    cam_x = mid(0, actor.pos.x*8-64, map_w*8-128)
    camera(cam_x, cam_y)
end

--------------------------------------------------
-- narration
--------------------------------------------------
nar_text = ""
nar_i = 1
nar_d = nil
function splitstring(x, char)
    ret = {}
    word = ""
    for i = 1, #x do
        local c = sub(x, i, i)
        if (c == char) then
            add(ret, word)
            word = ""
        else
            word = word..c
        end
    end
    if (#word) then
        add(ret, word)
    end
    return ret
end
function fit_to_lines(x, width)
    width = width or 28
    ret = ""
    local rest = x
    while (#rest != 0) do
        if (#rest <= width) then
            ret = ret..rest
            rest = ""
        else
            c = width
            while (sub(rest,c,c) != " " and sub(rest,c,c) != "\n") do
                c -= 1
            end
            ret = ret..sub(rest,0,c-1)..'\n'
            rest = sub(rest,c+1)
        end
    end
    return ret
end
function pumpnarration()
    if (nar_i >= #nar_text) then
        if (nar_d and not(nar_d.has_result)) nar_d.resolve(true) nar_i += 1
    else
        nar_i += 1
    end
end
function drawnarration()
    local x = 10
    local y = 10
    local c = 1
    local columns = 27
    for i = 1, nar_i do
        c = sub(nar_text,i,i)
        if (c == "\n") then
            y += 7
            x = 10
        else
            print(c, x, y, white)
            x += 4
        end
    end
end
function narrate(s)
    nar_text = fit_to_lines(s)
    nar_i = 1
    nar_d = deferred()
    return nar_d
end


--------------------------------------------------
-- game engine
--------------------------------------------------
state = {}
states = {}

timers = {}
function set_interval(seconds, func)
    local timer = {
        func=func,
        delay=seconds*30,
        rep=true,
    }
    timer.interval = timer.delay
    add(timers, timer)
    return timer
end
function set_timeout(seconds, func)
    local timer = set_interval(seconds, func)
    timer.rep = false
    return timer
end
function clear_timers()
    timers = {}
end
function pump_timers()
    for timer in all(timers) do
        if (timer.delay <= 0) then
            timer.func()
            timer.delay = timer.interval
            if (not(timer.rep)) del(timers, timer)
        else
            timer.delay -= 1
        end
    end
end
function wait(seconds)
    local d = deferred()
    set_timeout(seconds, d.resolve)
    return d
end

function _init()
    changetostate('escape')
end

function _draw()
    if (fatalmsg) then
        cls()
        cursor(0,0)
        color(white)
        print(fatalmsg)
        return
    end

    if (state and state.d_clear) then
        state.d_clear()
    else
        cls()
    end
    
    if (state and state.draw_background) state.draw_background()
    if (state and state.draw_actors) then
        state.draw_actors()
    else
        -- characters
        foreach(tethers, draw_tether)
        foreach(actors, draw_actor)
    end

    if (state and state.draw_foreground) state.draw_foreground()
    drawnarration()
end

function _update()
    pump_timers()
    if (state.update) state.update()

    done_collisions = {}
    foreach(tethers, constrain_tether)
    foreach(actors, move_actor)
    pumpnarration()
end

fatalmsg = nil
function fatal(msg)
    fatalmsg = msg
end

function addstate(data)
    -- name, init, update, draw, leave
    states[data.name] = data
end
function changetostate(name)
    if (not(states[name])) then
        fatal("no such state:"..name)
        return
    end
    if (state and state.leave) then
        state.leave()
    end
    state = states[name]
    state.init()
end


--------------------------------------------------
-- state: intro
--------------------------------------------------
addstate{
    name='error',
    draw=function()
        cls()
        print("there was an error")
    end,
}
addstate{
    name='intro',
    init=function()
        balloon = make_balloon(9, 8)
        balloon.accel.y = 0
        balloon.drawstring = true
    end,
    draw_foreground=function()
        color(white)
        print("the", 100, 72)
        print("proud", 92, 78)
        print("pink", 96, 84)
        print("balloon", 84, 90)
        print("theproudpinkballoon.com", 20, 110)
    end,
    update=function()
        if (btnp(4) or btnp(5)) then
            changetostate('chapter1')
        end
    end,
    leave=function()
        destroy_all_actors()
    end,
}

--------------------------------------------------
-- state: balloon-seller
--------------------------------------------------
addstate{
name='chapter1',
init=function()
    -- set_interval(5, function()
    --     bird = make_bird(-1, 1+rnd(4))
    --     bird.vel.y = -0.02
    -- end)
    -- set_interval(4.1, function()
    --     bird = make_bird(17, 1+rnd(5))
    --     bird.vel.x *= -1.5
    --     bird.vel.y = -0.01
    -- end)

    -- man = make_man(13, 13)
    -- man.facing = left
    -- foreach({dark_blue, orange, red, green, yellow}, function(color)
    --     b = make_balloon(6+rnd(3), 12)
    --     b.color = color
    --     make_tether(b, man, 2+rnd(1))
    -- end)

    man = make_man(20, 13)
    man.vel.x = -0.06
    man.inertia = 1
    man.land_inertia = 1
    man.air_inertia = 1
    man.beholden_to_walls = false
    man.control = function(man)
        if (man.pos.x <= 13) then
            -- let the player control
            man.vel.x = 0
            man.beholden_to_walls = true
            proudpink.beholden_to_walls = true
            proudpink.control = control_balloon

            local d = wait(2)
            thencall(d, narrate, "floated balloons colored\n"..
                                 "blue, orange, red, green\n"..
                                 "and yellow.")
            thencall(d, wait, 3)
            thencall(d, narrate, "just one pink balloon stood out bright in the crowd")
            thencall(d, wait, 3)
            thencall(d, narrate, "and pink's tail was longest\n"..
                                 "     which made pink"..
                                 "               quite proud")
            thencall(d, wait, 3)
            thencall(d, narrate, "")
            man.control = nil
        end
    end
    foreach({dark_blue, orange, red, green, yellow}, function(color)
        b = make_balloon(18+rnd(2), 12)
        b.color = color
        b.beholden_to_walls = false
        make_tether(b, man, 2+rnd(1))
    end)

    proudpink = make_balloon(18, 11)
    proudpink.beholden_to_walls = false
    tether = make_tether(proudpink, man, 5)
    tether.onbreak = function()
        destroy_tether(tether)
        proudpink.drawstring = true
    end

    narrate("in the bunch of balloons held by the balloon-selling fellow...")
end,
update=function()
end,
draw_background=function()
    -- sky
    rectfill(0, 0, 128, 128, blue)

    -- background 1
    map(0,3,0,97,16,1)

    -- ground
    map(0,0,0,101,16,3)
end,
draw_foreground=function()
end,
leave=function()
end,
}

--------------------------------------------------
-- state: escape
--------------------------------------------------
addstate{
name='escape',
init=function()
    choosemap(16,0,21,16)
    proudpink = make_balloon(2, 6)
    proudpink.control = control_balloon
    chase_target = proudpink
    girl1 = make_girl(3, 10)
    girl1.control = fsm_chaser(girl1)

    girl2 = make_girl(5, 10)
    girl2.control = fsm_chaser(girl2)
end,
update=function()
end,
draw_background=function()
    -- sky
    rectfill(0, 0, 128, 128, blue)

    -- level
    put_camera_on(proudpink)
    draw_chosen_map()
end,
draw_foreground=function()
end,
leave=function()
end,
}

--------------------------------------------------
-- state: flying
--------------------------------------------------
addstate{
name='flying',
init=function()
    bird = make_bird(-1, 2)
    bird = make_bird(17, 4.5)
    bird.vel.x *= -1.5
    bird.vel.y = -0.01
end,
update=function()
end,
draw_background=function()
    -- sky
    rectfill(0, 0, 128, 128, blue)

    -- background 1
    map(0,3,0,97,16,1)

    -- ground
    map(0,0,0,101,16,3)
end,
draw_foreground=function()
    displaynarration()
end,
leave=function()
end,
}


__gfx__
00ffff0000000000000700000007000000007000000000000000000000044440000444400000000000000000000000000f4444f0011110000000000000000000
001ff10000eeee0000070000000700000000700000000000000000000044fff00044fff0004444000044440000444400081ff180077770000000000000000000
00ffff000eee7ee00007000000070000000070000000077000000000088f1f10088f1f1004ffff0004ffff0004ffff0008ffff80011111000000000000000000
f999999f0eeee7e00007000000007000000700000007777770000000044ffff0044ffff0081ff100081ff100081ff10008ffff8004f3f0000000000000000000
009999000eeeeee00007000000007000000700000077777770000000040888800408888004cffc00047ffc0004cff7000088880004fff0000000000000000000
00aaaa000eeeeee00007000000000700007000000f777777f7770000000888800008888004c8870004c88c0004788c000088880004fff0000000000000000000
00a00a0000eeee00000700000000007007000000fffff77777777000000888800000880040788c0440c8870440c88c0400888800007700000000000000000000
00800800000ee000000000000000000000000000ffffffff7777770000040040000044004f8888f44f8888f44f8888f400400400011110000000000000000000
00000000bbbbbbbbbbbbbbbb333333330000000000000000000000000000000000000000004444f00f4444000400000000000000011110000111100000000000
000000003bb3bbbb3bbbb3333333333300000000000000000000333000000000000000000f4444800844ff0f444044440000000001111f0001111f0000000000
000000003333333333333333333333330044444444444400000333333000000000000300084884800888f180400844ff00000000011110000111100000000000
000000003333333333333333333333330005000000005000003333233330000000a0030008844880084fff8000004ff100000000077770000777700000000000
666666563333333333333333333333330044444444444400033333333230033000b03000008488000448880048888fff00000444011110000111100000000000
6666656633333333333333333333333300050000000050000323333333333330000b300a008488000488880008888800044844ff010010000011000000000000
6666566633333333333333333333333304444444444444403333323333333233000b30b000880400004088004888808f48884ff1040040000044000000000000
6665666633333333333333333333333300050000000050003333333333333333000b3b0000400000000004000000000048888fff044044000044400000000000
00000000000ee000000e000000000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
0000000000ee7e0000e0000000000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
000000000eee0ee000e0000e00000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
000000000ee0007ee000007e00000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
000000000e0000eee000e0e000000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
000000000ee00ee00e000e0000000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
0000000000eeee000e00000000000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
00000000000ee00000ee000000000000333333331111111166666666000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00003333333333330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00333333333333330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03333333333333330000000000000000000000000000000000000000000044000000440000000000000000000000000000000000000000000000000000000000
33333333333333330000000000000000000000000000000000000000409441404044414000000000000000000000000000000000000000000000000000000000
33333333333333330000000000000000000000000000000000000000449944aa444444aa00000000000000000000000000000000000000000000000000000000
33333333333333330000000000000000000000000000000000000000000000000099000000000000000000000000000000000000000000000000000000000000
33333333333333330000000000000000000000000000000000000000000000000090000000000000000000000000000000000000000000000000000000000000
03333333333333330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000544454440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000454454540000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000454454440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000454445440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000544545440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000544545440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000544544440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000544544540000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00ffff0000ffff006666666633b333b3004444000044440055555555667666760000000000000000000000000000000000000000000000000000000000000000
001ff100001ff1006666666633333333001441000014410055555555666666660000000000000000000000000000000000000000000000000000000000000000
00ffff0000ffff006566666655555555004444000044440051555555111111110000000000000000000000000000000000000000000000000000000000000000
f888888ff888888f6666666666666666499999944999999455555555555555550000000000000000000000000000000000000000000000000000000000000000
00888800008888006666666666666666009999000099990555555555555555550000000000000000000000000000000000000000000000000000000000000000
00eeee000eeeeee0666665666666666600aaaa000aaaaaa055555155555555550000000000000000000000000000000000000000000000000000000000000000
00e00e0040000004666666666666666600a00a008000000855555555555555550000000000000000000000000000000000000000000000000000000000000000
00400400000000006666666666666666008008000000000055555555555555550000000000000000000000000000000000000000000000000000000000000000

__gff__
0000000000000000000000000000000000010101000000000000000000000000000000000200040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
1010101010101010101010101010101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1211121211111112121112111112121200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1313131313131313131313131313131300000000000000000000000000002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000161700000000000014150018000000000000000000000000000000002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000004041000000000000000000000000000000000000000000000000002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00404141510000002b2b2b2b0000000000000000000000000000000000002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000515151002727282828282a00000000000000000000000000000000002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000051515100272729292929292a000000000000000000002400000024002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000262400000024002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000262424242424002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000260025252500002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000260025252500002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000260025252500002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000260025252500002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000260025252500002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000024242424242424242424242424242424242424242400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100001c4501a44014440104400b420064100341001410014100e4000e4000e4002640003400034000640007400094000940009400084000640003400054000540004400024000240002400014000140000400
00010000203361c3261a32617326133260f3160b31605316013160d3060b3060c3060d3060e3061030614306183061e3061f30626306273062a3062c3062e3062b3062c306333063330630306393062d30629306
000000000000037050370501a050131202005037050191500000019150000000c1500000018150000000b1502215029150291501d150000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141
00 41414141

