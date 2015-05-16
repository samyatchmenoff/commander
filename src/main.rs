//#![license = "MIT"]

extern crate rand;
extern crate piston;
extern crate graphics;
extern crate glfw_window;
extern crate opengl_graphics;
extern crate cgmath;
extern crate openal;
extern crate vorbisfile;

use std::cmp::min;
use std::f64::consts::FRAC_2_PI;
use std::i16;
use std::iter::FromIterator;
use std::iter::Iterator;
use std::collections::{HashMap,HashSet};
use std::default::Default;
use std::hash::{Hash,Hasher};
use std::fs::File;
use rand::random;
use piston::window::{WindowSettings, Size};
use piston::event::*;
use graphics::*;
use cgmath::{Vector, Vector2, EuclideanVector, zero};
use cgmath::Zero;
use cgmath::Point;
use cgmath::{Aabb, Aabb2};
use cgmath::rad;
use openal::al;
use openal::alc;
use vorbisfile as vorbis;

static MISSILE_COOLDOWN: u32 = 60;
static MISSILE_LIFETIME: u32 = 90;
static MISSILE_SHOOT_THRESHOLD: f64 = 200.0;
static FOLLOW_DISTANCE: f64 = 100.0;
static FLOCK_DISTANCE: f64 = 100.0;
static SELECT_RADIUS: f64 = 25.0;
static SPEED: f64 = 50.0;

#[derive(Debug,Clone)]
struct ShipInput {
  thrust: f64,
  steer: f64,
  shoot: Option<u32>,
  target_pos: Vector2<f64>
}

impl std::default::Default for ShipInput {
  fn default() -> ShipInput {
    ShipInput { thrust: 0.0, steer: 0.0, shoot: None, target_pos: zero() }
  }
}

#[derive(Debug)]
struct Missile {
  pos: Vector2<f64>,
  vel: Vector2<f64>,
  target_ship_id: u32,
  age: u32
}

impl Missile {
  fn new(pos: Vector2<f64>, target_ship_id: u32) -> Missile {
    Missile {
      pos: pos,
      vel: zero(),
      target_ship_id: target_ship_id,
      age: 0
    }
  }
}

#[derive(Debug)]
struct Ship {
  name: String,
  owner: String,
  hull_health: u32,
  shield_health: u32,
  missile_cooldown: u32,
  pos: Vector2<f64>,
  rot: f64,
  speed: f64,
  command: Command
}

impl Ship {
  fn new(name: &str, owner: &str, pos: Vector2<f64>, rot: f64) -> Ship {
    Ship {
      name: name.to_string(),
      owner: owner.to_string(),
      hull_health: 100,
      shield_health: 100,
      missile_cooldown: MISSILE_COOLDOWN,
      pos: pos,
      rot: rot,
      speed: SPEED,
      command: Command::Goto(pos)
    }
  }

  fn vel(&self) -> Vector2<f64> {
    Vector2::new(self.speed * self.rot.cos(), self.speed * self.rot.sin())
  }
}

#[derive(Debug,Clone)]
enum Command {
  Goto(Vector2<f64>),     // (pos)
  Follow(u32),           // (target_ship_id)
  Attack(u32),           // (target_ship_id)
}
impl Copy for Command {}

#[derive(Debug)]
struct Universe {
  pub ships: HashMap<u32,Ship>,
  pub missiles: Vec<Missile>
}

impl<'r> Universe {
  fn new() -> Universe {
    Universe {
      ships:
        (0..10).map(|i| {
          let (name, owner) = if i % 2 == 0 { ("Player", "player") } else { ("Enemy", "enemy") };
          (i, Ship::new(name,
                        owner,
                        Vector2::new(rand::random::<f64>() * 700.0 + 100.0,
                                     rand::random::<f64>() * 600.0 + 100.0),
                        rand::random::<f64>() * FRAC_2_PI))
        }).collect::<HashMap<u32,Ship>>(),
      missiles: Vec::new()
    }
  }
}

struct InputState {
  mouse_position: Vector2<f64>,
  mouse_press_position: Vector2<f64>,
  mouse_button_state: HashMap<piston::input::MouseButton,bool>
}

impl InputState {
  fn new() -> InputState {
    InputState {
      mouse_position: zero(),
      mouse_press_position: zero(),
      mouse_button_state: HashMap::new()
    }
  }
}

enum GameState {
  Paused,
  Simulating,
}

pub struct App {
  game_state: GameState,
  universe: Universe,
  selected_ships: HashSet<u32>,
  input_state: InputState,
  gl: opengl_graphics::GlGraphics,
  al_device: alc::Device,
  al_ctx: alc::Context,
  colors: HashMap<String,(f32,f32,f32)>,
  sounds: HashMap<String,al::Source>
}

impl App {
  fn new() -> App {
    let al_device = alc::Device::open(None).expect("Could not open audio device");
    let al_ctx = al_device.create_context(&[]).expect("Could not create OpenAL context");
    al_ctx.make_current();

    let mut colors = HashMap::new();
    colors.insert("player".to_string(), (1.0, 1.0, 0.3));
    colors.insert("enemy".to_string(), (1.0, 0.3, 0.3));
    let mut sounds = HashMap::new();
    sounds.insert("shoot".to_string(), load_sound("shoot.ogg"));

    App {
      game_state: GameState::Simulating,
      universe: Universe::new(),
      selected_ships: HashSet::new(),
      input_state: InputState::new(),
      gl: opengl_graphics::GlGraphics::new(opengl_graphics::OpenGL::_3_2),
      al_device: al_device,
      al_ctx: al_ctx,
      colors: colors,
      sounds: sounds
    }
  }
}

impl App {
  fn update(&mut self) {
    match self.game_state {
      GameState::Paused => {},
      GameState::Simulating => {
        let mut did_shoot = false;

        let ship_inputs: HashMap<u32,ShipInput> = HashMap::from_iter(
          self.universe.ships.iter().map(|(&ship_id, ship)| {
            let (target_pos, shoot) = match ship.command {
              Command::Goto(pos) => {
                (pos, None)
              }
              Command::Follow(target_ship_id) => {
                (self.universe.ships.get(&target_ship_id).map(|s| s.pos).unwrap_or(ship.pos), None)
              }
              Command::Attack(enemy_ship_id) => {
                let pos = self.universe.ships.get(&enemy_ship_id).map(|s| s.pos).unwrap_or(ship.pos);
                if (ship.pos - pos).length2() < MISSILE_SHOOT_THRESHOLD*MISSILE_SHOOT_THRESHOLD {
                  (pos, Some(enemy_ship_id))
                } else {
                  (pos, None)
                }
              }
            };

            let seperation_v = self.universe.ships.iter().filter_map(|(&other_id, other)| {
              let dist2 = (ship.pos - other.pos).length2();
              if other_id == ship_id || dist2 > FLOCK_DISTANCE*FLOCK_DISTANCE {
                None
              } else {
                Some((ship.pos - other.pos).div_s(dist2))
              }
            }).fold(zero(), |b,a| a + b);

            let alignment_v = self.universe.ships.iter().filter_map(|(&other_id, other)| {
              let dist2 = (ship.pos - other.pos).length2();
              if other_id == ship_id || dist2 > FLOCK_DISTANCE*FLOCK_DISTANCE {
                None
              } else {
                Some((other.vel() - ship.vel()).div_s(dist2))
              }
            }).fold(zero(), |b,a| a + b);

            let cohesion_v: Vector2<f64> =
              self.universe.ships.iter().filter_map(|(&other_id, other)| {
                let dist2 = (ship.pos - other.pos).length2();
                if other_id == ship_id || dist2 > FLOCK_DISTANCE*FLOCK_DISTANCE {
                  None
                } else {
                  Some(Vector2::zero())//None//Some((other.vel - ship.vel).div_s(dist2))
                }
              }).fold(zero(), |b,a| a + b);

            let goal_dist2 = (target_pos - ship.pos).length2();
            let goal_v: Vector2<f64> = if goal_dist2 > 1.0 {
              (target_pos - ship.pos)
            } else {
              zero()
            };
      
            let steer_v = seperation_v.mul_s(200.0) +
                          alignment_v.mul_s(50.0) +
                          cohesion_v.mul_s(50.0) +
                          goal_v.mul_s(1.0);

            let steer = ship.vel().angle(&steer_v) + rad(rand::random::<f64>() * 0.1);

            let thrust = 0.0;

            (ship_id, ShipInput {
              thrust: thrust,
              steer: steer.s,
              shoot: shoot,
              target_pos: zero()
            })
          })
        );

        let Universe { ships: ref mut ships, missiles: ref mut missiles } = self.universe;

        for (ship_id, input) in ship_inputs.iter() {
          if let Some(ship) = ships.get_mut(ship_id) {
            ship.missile_cooldown = ship.missile_cooldown.checked_sub(1).unwrap_or(0);
            ship.speed = ship.speed + input.thrust / 60.0;
            ship.rot = ship.rot + input.steer / 60.0;
            ship.pos = ship.pos + ship.vel().div_s(60.0);
            match (input.shoot, ship.missile_cooldown == 0) {
              (Some(target_ship_id), true) => {
                did_shoot = true;
                missiles.push(Missile::new(ship.pos, target_ship_id));
                ship.missile_cooldown = MISSILE_COOLDOWN;
              }
              _ => {}
            };
          };
        }

        for m in missiles.iter_mut() {
          match ships.get(&m.target_ship_id) {
            Some(target_ship) => {
              let diff = target_ship.pos - m.pos;
              let dist = diff.length();
              if dist > 10.0 {
                m.vel = m.vel.mul_s(0.8) + diff.mul_s(2.0 * SPEED / dist);
                m.pos = m.pos + m.vel.div_s(60.0);
                m.age += 1;
              } else {
                m.age = MISSILE_LIFETIME;
              }
            }
            None => {
              m.age = MISSILE_LIFETIME;
            }
          };
        }

        missiles.retain(|m| { m.age < MISSILE_LIFETIME });

        if did_shoot {
          self.sounds.get_mut(&"shoot".to_string()).unwrap().play();
        }
      }
    }
  }

  fn render(&mut self, args: &piston::event::RenderArgs) {
    self.gl.viewport(0, 0, 2 * args.width as i32, 2 * args.height as i32);

    let mut c = Context::abs(args.width as f64, args.height as f64);
    c.draw_state = c.draw_state.blend(draw_state::BlendPreset::Alpha);
    
    clear([0.1, 0.1, 0.2, 1.0], &mut self.gl);

    for ship in self.universe.ships.values() {
      let colors = &self.colors;
      let (r,g,b) = colors.get(&ship.owner).unwrap_or(&(1.0, 1.0, 1.0)).clone();

      rectangle([r,g,b,1.0], [ship.pos.x - 5.0, ship.pos.y - 5.0, 10.0, 10.0], c.transform, &mut self.gl);
      //c.rect(ship.pos.x - 5.0, ship.pos.y - 5.0, 10.0, 10.0).rgb(r, g, b).draw(&mut self.gl);

      match ship.command {
        Command::Goto(pos) => {
          line::Line::new([0.5, 0.5, 1.0, 1.0], 1.0)
            .draw([ship.pos.x, ship.pos.y, pos.x, pos.y], &c.draw_state, c.transform, &mut self.gl);
        }
        Command::Follow(target_ship_id) => {
          let p = self.universe.ships.get(&target_ship_id).map(|s| s.pos);
          match p {
            Some(pos) => {
              line::Line::new([0.5, 1.0, 0.5, 1.0], 1.0)
                .draw(
                  [ship.pos.x, ship.pos.y, pos.x, pos.y],
                  &c.draw_state,
                  c.transform,
                  &mut self.gl);
            }
            None => {}
          }
        }
        Command::Attack(target_ship_id) => {
          if let Some(pos) = self.universe.ships.get(&target_ship_id).map(|s| s.pos) {
            line::Line::new([1.0, 0.5, 0.5, 1.0], 1.0)
              .draw(
                [ship.pos.x, ship.pos.y, pos.x, pos.y],
                &c.draw_state,
                c.transform,
                &mut self.gl);
          }
        }
      }
    }

    for ship_id in self.selected_ships.iter() {
      if let Some(ship) = self.universe.ships.get(ship_id) {
        ellipse::Ellipse::new([0.7, 1.0, 0.7, 0.4])
          .draw(
            [ship.pos.x - SELECT_RADIUS, ship.pos.y - SELECT_RADIUS, 2.0 * SELECT_RADIUS, 2.0 * SELECT_RADIUS],
            &c.draw_state,
            c.transform,
            &mut self.gl);
      }
    }

    match ship_at_pos(self.universe.ships.iter(), self.input_state.mouse_position) {
      Some((_, ship)) => {
        //c.rect(ship.pos.x - SELECT_RADIUS, ship.pos.y - SELECT_RADIUS, SELECT_RADIUS * 2.0, SELECT_RADIUS * 2.0)
        //  .rgba(0.7, 0.5, 1.0, 0.3)
        //  .draw(&mut self.gl);
      }
      None => {}
    };

    for m in self.universe.missiles.iter() {
      ellipse::Ellipse::new([1.0, 1.0, 1.0, 1.0])
        .draw(
          [m.pos.x, m.pos.y, m.pos.x + 5.0, m.pos.y + 5.0],
          &c.draw_state,
          c.transform,
          &mut self.gl);
    }

    if self.input_state.mouse_button_state.get_or_default(&piston::input::MouseButton::Left) == true {
      let select_rect: Aabb2<f64> = Aabb2::new(
        Point::from_vec(&self.input_state.mouse_position),
        Point::from_vec(&self.input_state.mouse_press_position)
      );
      for ship in self.universe.ships.values() {
        if ship.owner == "player" && select_rect.contains(&Point::from_vec(&ship.pos)) {
          rectangle::Rectangle::new([0.7, 0.5, 1.0, 0.3])
            .draw(
              [
                ship.pos.x - SELECT_RADIUS,
                ship.pos.y - SELECT_RADIUS,
                2.0 * SELECT_RADIUS,
                2.0 * SELECT_RADIUS
              ],
              &c.draw_state, c.transform, &mut self.gl);
        }
      }
      rectangle::Rectangle::new_round([0.5, 0.5, 0.6, 0.5], 5.0)
        .draw(
          [
            self.input_state.mouse_position.x,
            self.input_state.mouse_position.y,
            self.input_state.mouse_press_position.x - self.input_state.mouse_position.x,
            self.input_state.mouse_press_position.y - self.input_state.mouse_position.y
          ],
          &c.draw_state, c.transform, &mut self.gl);
    }
  }

  fn key_press(&mut self, key: piston::input::keyboard::Key) {
    match (&self.game_state, key) {
      (&GameState::Paused, piston::input::keyboard::Key::Space) => {
        self.game_state = GameState::Simulating;
      }
      (&GameState::Simulating, piston::input::keyboard::Key::Space) => {
        self.game_state = GameState::Paused;
      }
      _ => {}
    }
  }

  fn mouse_release(&mut self, button: piston::input::MouseButton) {
    self.input_state.mouse_button_state.insert(button, false);

    if button == piston::input::MouseButton::Left {
      let select_rect: Aabb2<f64> = Aabb2::new(
        Point::from_vec(&self.input_state.mouse_position),
        Point::from_vec(&self.input_state.mouse_press_position)
      );

      self.selected_ships = self.universe.ships.iter().filter_map(|(ship_id, ship)| {
        if ship.owner == "player" && select_rect.contains(&Point::from_vec(&ship.pos)) {
          Some(*ship_id)
        } else {
          None
        }
      }).collect();
    }
  }

  fn mouse_press(&mut self, button: piston::input::MouseButton) {
    self.input_state.mouse_button_state.insert(button, true);
    self.input_state.mouse_press_position = self.input_state.mouse_position;

    let pos = self.input_state.mouse_position;

    if button == piston::input::MouseButton::Right {
      let command = {
        let selected = ship_at_pos(self.universe.ships.iter(), pos);
        match selected {
          Some((&new_ship_id, new_ship)) => {
            if new_ship.owner == "player" {
              Command::Follow(new_ship_id)
            } else {
              Command::Attack(new_ship_id)
            }
          }
          None => {
            Command::Goto(self.input_state.mouse_position)
          }
        }
      };

      for ship_id in self.selected_ships.iter() {
        if let Some(s) = self.universe.ships.get_mut(ship_id) {
          s.command = command;
        }
      }
    }
  }

  fn mouse_move(&mut self, x: f64, y: f64) {
    self.input_state.mouse_position = Vector2::new(x, y);
  }
}

fn ship_at_pos<'r, I: Iterator<Item=(&'r u32, &'r Ship)>>(iter: I, pos: Vector2<f64>) -> Option<(&'r u32, &'r Ship)> {
  iter.filter_map(|(ship_id, ship)| {
    let dist2 = (ship.pos - pos).length2();
    if dist2 < SELECT_RADIUS*SELECT_RADIUS {
      Some((ship_id, ship, dist2))
    } else {
      None
    }
  })
    .fold(None, |min, (ship_id, ship, dist2)| {
      match min {
        Some((nearest_id, nearest_ship, nearest_dist2)) => {
          if dist2 < nearest_dist2 {
            Some((ship_id, ship, dist2))
          } else {
            Some((nearest_id, nearest_ship, nearest_dist2))
          }
        }
        None => Some((ship_id, ship, dist2))
      }
    })
    .map(|(ship_id, ship, _)| (ship_id, ship))
}

fn main() {
  let mut window = glfw_window::GlfwWindow::new(
    opengl_graphics::OpenGL::_3_2,
    WindowSettings::new("Commander".to_string(), Size { width: 800, height: 600 })
  );

  let mut app = App::new();

  for e in window.events() {
    match e {
      Event::Idle(_) => (),
      Event::Render(args) => app.render(&args),
      Event::AfterRender(_) => (),
      Event::Update(_) => app.update(),
      Event::Input(piston::input::Input::Press(piston::input::Button::Keyboard(key))) => app.key_press(key),
      Event::Input(piston::input::Input::Press(piston::input::Button::Mouse(button))) => app.mouse_press(button),
      Event::Input(piston::input::Input::Release(piston::input::Button::Mouse(button))) => app.mouse_release(button),
      Event::Input(piston::input::Input::Move(piston::input::Motion::MouseCursor(x, y))) => app.mouse_move(x, y),
      Event::Input(_) => ()
    }
  }
}

trait GetOrDefault<K,V> {
  fn get_or_default(&self, key: &K) -> V;
}

impl<K: Hash + Eq, V: Default + Clone> GetOrDefault<K,V> for HashMap<K,V> {
  fn get_or_default(&self, key: &K) -> V {
    self.get(&key).cloned().unwrap_or_default()
  }
}

fn load_sound(filename: &str) -> al::Source {
  //use std::slice::Slice;
  let mut stream = File::open(filename).unwrap();
  let mut vf = vorbis::VorbisFile::new(stream).unwrap();
  let mut samples: Vec<i16> = Vec::new();
  loop {
    match vf.decode() {
      Ok(channels) => {
        samples.extend(
          channels[0].iter().map(|x| (x * (i16::MAX - 1) as f32) as i16).collect::<Vec<i16>>()
        );
      }
      _ => {
        break;
      }
    }
  }
  let buffer = al::Buffer::gen();
  let sample_freq: f32 = 44100.0;
  unsafe { buffer.buffer_data(al::Format::Mono16, &*samples, sample_freq as al::ALsizei) };

  let source = al::Source::gen();
  source.queue_buffer(&buffer);
  return source;
}

fn pmin<T: PartialOrd>(v1: T, v2: T) -> T {
  if v1.lt(&v2) { v1 } else { v2 }
}

fn pmax<T: PartialOrd>(v1: T, v2: T) -> T {
  if v2.lt(&v1) { v1 } else { v2 }
}
