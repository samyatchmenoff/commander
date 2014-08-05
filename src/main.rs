#![license = "MIT"]

#![feature(globs)]
#![feature(default_type_params)]

extern crate graphics;
extern crate piston;
extern crate glfw_game_window;
extern crate opengl_graphics;
extern crate cgmath;
extern crate openal;
extern crate libvorbisfile;

use std::i16;
use std::rand;
use std::iter::FromIterator;
use std::collections::{HashMap,HashSet};
use std::default::Default;
use std::hash::{Hash,Hasher};
use std::io::File;
use graphics::*;
use piston::{Game, GameWindow};
use cgmath::vector::{Vector, Vector2, EuclideanVector};
use cgmath::point::Point;
use cgmath::aabb::{Aabb, Aabb2};
use openal::al;
use openal::alc;
use vorbis = libvorbisfile;

static MISSILE_COOLDOWN: uint = 60;
static MISSILE_LIFETIME: uint = 90;
static MISSILE_SHOOT_THRESHOLD: f64 = 200.0;
static SELECT_RADIUS: f64 = 25.0;
static SPEED: f64 = 50.0;

#[deriving(Show,Clone)]
struct ShipInput {
  thrust: Vector2<f64>,
  shoot: Option<uint>,
  target_pos: Vector2<f64>
}

impl std::default::Default for ShipInput {
  fn default() -> ShipInput {
    ShipInput { thrust: Vector2::zero(), shoot: None, target_pos: Vector2::zero() }
  }
}

#[deriving(Show)]
struct Missile {
  pos: Vector2<f64>,
  vel: Vector2<f64>,
  target_ship_id: uint,
  age: uint
}

impl Missile {
  fn new(pos: Vector2<f64>, target_ship_id: uint) -> Missile {
    Missile {
      pos: pos,
      vel: Vector2::zero(),
      target_ship_id: target_ship_id,
      age: 0
    }
  }
}

#[deriving(Show)]
struct Ship {
  name: String,
  owner: String,
  hull_health: uint,
  shield_health: uint,
  missile_cooldown: uint,
  pos: Vector2<f64>,
  vel: Vector2<f64>,
  command: Option<cmd::Command>
}

impl Ship {
  fn new(name: &str, owner: &str, pos: Vector2<f64>) -> Ship {
    Ship {
      name: name.to_string(),
      owner: owner.to_string(),
      hull_health: 100,
      shield_health: 100,
      missile_cooldown: MISSILE_COOLDOWN,
      pos: pos,
      vel: Vector2::new(0.0, 0.0),
      command: None
    }
  }
}

mod cmd {
  use cgmath::vector::Vector2;

  #[deriving(Show)]
  pub enum Command {
    Goto(Vector2<f64>),     // (pos)
    Follow(uint),           // (target_ship_id)
    Attack(uint),           // (target_ship_id)
  }
}

#[deriving(Show)]
struct Universe {
  pub ships: HashMap<uint,Ship>,
  pub missiles: Vec<Missile>
}

impl<'r> Universe {
  fn new() -> Universe {
    Universe {
      ships: FromIterator::from_iter(
        Vec::from_fn(10, |i| {
          let (name, owner) = if i % 2 == 0 { ("Player", "player") } else { ("Enemy", "enemy") };
          (i, Ship::new(name, owner, Vector2::new(rand::random::<f64>() * 600.0 + 100.0, rand::random::<f64>() * 400.0 + 100.0)))
        }).move_iter()
      ),
      missiles: Vec::new()
    }
  }
}

struct InputState {
  mouse_position: Vector2<f64>,
  mouse_press_position: Vector2<f64>,
  mouse_button_state: HashMap<piston::mouse::Button,bool>
}

impl InputState {
  fn new() -> InputState {
    InputState {
      mouse_position: Vector2::zero(),
      mouse_press_position: Vector2::zero(),
      mouse_button_state: HashMap::new()
    }
  }
}

enum GameState {
  Paused,
  Simulating,
}

pub struct App<'r> {
  game_state: GameState,
  universe: Universe,
  selected_ships: HashSet<uint>,
  input_state: InputState,
  gl: opengl_graphics::Gl,
  al_device: alc::Device,
  al_ctx: alc::Context,
  colors: HashMap<String,(f32,f32,f32)>,
  sounds: HashMap<String,al::Source>
}

impl<'r> App<'r> {
  fn new() -> App<'r> {
    let al_device = alc::Device::open(None).expect("Could not open audio device");
    let al_ctx = al_device.create_context([]).expect("Could not create OpenAL context");
    al_ctx.make_current();

    let mut colors = HashMap::new();
    colors.insert("player".to_string(), (1.0, 1.0, 0.3));
    colors.insert("enemy".to_string(), (1.0, 0.3, 0.3));
    let mut sounds = HashMap::new();
    sounds.insert("shoot".to_string(), load_sound("shoot.ogg"));

    App {
      game_state: Simulating,
      universe: Universe::new(),
      selected_ships: HashSet::new(),
      input_state: InputState::new(),
      gl: opengl_graphics::Gl::new(),
      al_device: al_device,
      al_ctx: al_ctx,
      colors: colors,
      sounds: sounds
    }
  }
}

impl<'r, W: GameWindow> piston::Game<W> for App<'r> {
  fn update(&mut self, _window: &mut W, _args: &piston::UpdateArgs) {
    match self.game_state {
      Paused => {},
      Simulating => {
        let mut did_shoot = false;

        let ship_inputs: HashMap<uint,ShipInput> = FromIterator::from_iter(
          self.universe.ships.iter().map(|(&ship_id, ship)| {
            let (target_pos, shoot) = match ship.command {
              Some(cmd::Goto(pos)) => {
                (pos, None)
              }
              Some(cmd::Follow(target_ship_id)) => {
                (self.universe.ships.find(&target_ship_id).map(|s| s.pos).unwrap_or(ship.pos), None)
              }
              Some(cmd::Attack(enemy_ship_id)) => {
                let pos = self.universe.ships.find(&enemy_ship_id).map(|s| s.pos).unwrap_or(ship.pos);
                if (ship.pos - pos).length2() < MISSILE_SHOOT_THRESHOLD*MISSILE_SHOOT_THRESHOLD {
                  (pos, Some(enemy_ship_id))
                } else {
                  (pos, None)
                }
              }
              _ => (ship.pos, None)
            };

            let min_dist = match ship.command {
              Some(cmd::Attack(_)) => MISSILE_SHOOT_THRESHOLD * 0.75,
              Some(cmd::Follow(_)) => 50.0,
              _ => 10.0
            };

            let diff = target_pos - ship.pos;
            let dist = diff.length();
            let thrust = if dist < min_dist { Vector2::new(0.0, 0.0) } else { diff.mul_s(SPEED / dist) };
            (ship_id, ShipInput {
              thrust: thrust,
              shoot: shoot,
              target_pos: Vector2::zero()
            })
          })
        );

        let Universe { ships: ref mut ships, missiles: ref mut missiles } = self.universe;

        for (ship_id, input) in ship_inputs.iter() {
          ships.find_mut(ship_id).with(|ship| {
            ship.missile_cooldown = ship.missile_cooldown.checked_sub(&1).unwrap_or(0);
            ship.vel = ship.vel.mul_s(0.8) + input.thrust;
            ship.pos = ship.pos + ship.vel.div_s(60.0);
            match (input.shoot, ship.missile_cooldown == 0) {
              (Some(target_ship_id), true) => {
                did_shoot = true;
                missiles.push(Missile::new(ship.pos, target_ship_id));
                ship.missile_cooldown = MISSILE_COOLDOWN;
              }
              _ => {}
            };
          });
        }

        for m in missiles.mut_iter() {
          match ships.find(&m.target_ship_id) {
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
          self.sounds.find_mut(&"shoot".to_string()).unwrap().play();
        }
      }
    }
  }

  fn render(&mut self, _window: &mut W, args: &piston::RenderArgs) {
    self.gl.viewport(0, 0, 2 * args.width as i32, 2 * args.height as i32);

    let ref c = Context::abs(args.width as f64, args.height as f64);

    c.rgb(0.1, 0.1, 0.2).draw(&mut self.gl);

    for ship in self.universe.ships.values() {
      let colors = &self.colors;
      let (r,g,b) = colors.find(&ship.owner).unwrap_or(&(1.0, 1.0, 1.0)).clone();
      c.rect(ship.pos.x - 5.0, ship.pos.y - 5.0, 10.0, 10.0).rgb(r, g, b).draw(&mut self.gl);

      match ship.command {
        Some(cmd::Goto(pos)) => {
          c.line(ship.pos.x, ship.pos.y, pos.x, pos.y)
            .bevel_border_radius(1.0)
            .rgb(0.5, 0.5, 1.0)
            .draw(&mut self.gl);
        }
        Some(cmd::Follow(target_ship_id)) => {
          let p = self.universe.ships.find(&target_ship_id).map(|s| s.pos);
          match p {
            Some(pos) => {
              c.line(ship.pos.x, ship.pos.y, pos.x, pos.y)
                .bevel_border_radius(1.0)
                .rgb(0.5, 1.0, 0.5)
                .draw(&mut self.gl);
            }
            None => {}
          }
        }
        Some(cmd::Attack(target_ship_id)) => {
          let p = self.universe.ships.find(&target_ship_id).map(|s| s.pos);
          match p {
            Some(pos) => {
              c.line(ship.pos.x, ship.pos.y, pos.x, pos.y)
                .bevel_border_radius(1.0)
                .rgb(1.0, 0.5, 0.5)
                .draw(&mut self.gl);
            }
            None => {}
          }
        }
        None => {}
      };
    }

    for ship_id in self.selected_ships.iter() {
      match self.universe.ships.find(ship_id) {
        Some(ship) => {
          c.circle(ship.pos.x, ship.pos.y, SELECT_RADIUS).rgba(0.7, 1.0, 0.7, 0.4).draw(&mut self.gl);
        }
        None => {}
      }
    }

    match ship_at_pos(self.universe.ships.iter(), self.input_state.mouse_position) {
      Some((_, ship)) => {
        c.rect(ship.pos.x - SELECT_RADIUS, ship.pos.y - SELECT_RADIUS, SELECT_RADIUS * 2.0, SELECT_RADIUS * 2.0)
          .rgba(0.7, 0.5, 1.0, 0.3)
          .draw(&mut self.gl);
      }
      None => {}
    };

    for m in self.universe.missiles.iter() {
      c.circle(m.pos.x, m.pos.y, 5.0).rgba(1.0, 1.0, 1.0, 1.0).draw(&mut self.gl);
    }

    if self.input_state.mouse_button_state.find_or_default(&piston::mouse::Left) == true {
      let select_rect: Aabb2<f64> = Aabb2::new(
        Point::from_vec(&self.input_state.mouse_position),
        Point::from_vec(&self.input_state.mouse_press_position)
      );
      for ship in self.universe.ships.values() {
        if ship.owner.as_slice() == "player" && select_rect.contains(&Point::from_vec(&ship.pos)) {
          c.rect(ship.pos.x - SELECT_RADIUS, ship.pos.y - SELECT_RADIUS, SELECT_RADIUS * 2.0, SELECT_RADIUS * 2.0)
            .rgba(0.7, 0.5, 1.0, 0.3)
            .draw(&mut self.gl);
        }
      }
      c.rect(
        self.input_state.mouse_position.x,
        self.input_state.mouse_position.y,
        self.input_state.mouse_press_position.x - self.input_state.mouse_position.x,
        self.input_state.mouse_press_position.y - self.input_state.mouse_position.y
      ).rgba(0.5, 0.5, 0.6, 0.5).draw(&mut self.gl);
      c.rect(
        self.input_state.mouse_position.x,
        self.input_state.mouse_position.y,
        self.input_state.mouse_press_position.x - self.input_state.mouse_position.x,
        self.input_state.mouse_press_position.y - self.input_state.mouse_position.y
      ).border_radius(1.0).rgba(0.5, 0.5, 0.6, 1.0).draw(&mut self.gl);
    }
  }

  fn key_press(&mut self, _window: &mut W, args: &piston::KeyPressArgs) {
    match (self.game_state, args.key) {
      (Paused, piston::keyboard::Space) => {
        self.game_state = Simulating;
      }
      (Simulating, piston::keyboard::Space) => {
        self.game_state = Paused;
      }
      _ => {}
    }
  }

  fn mouse_release(&mut self, _window: &mut W, args: &piston::MouseReleaseArgs) {
    self.input_state.mouse_button_state.insert(args.button, false);

    if args.button == piston::mouse::Left {
      let select_rect: Aabb2<f64> = Aabb2::new(
        Point::from_vec(&self.input_state.mouse_position),
        Point::from_vec(&self.input_state.mouse_press_position)
      );

      self.selected_ships = self.universe.ships.iter().filter_map(|(ship_id, ship)| {
        if ship.owner.as_slice() == "player" && select_rect.contains(&Point::from_vec(&ship.pos)) {
          Some(*ship_id)
        } else {
          None
        }
      }).collect();
    }
  }

  fn mouse_press(&mut self, _window: &mut W, args: &piston::MousePressArgs) {
    self.input_state.mouse_button_state.insert(args.button, true);
    self.input_state.mouse_press_position = self.input_state.mouse_position;

    let pos = self.input_state.mouse_position;

    if args.button == piston::mouse::Right {
      let command = {
        let selected = ship_at_pos(self.universe.ships.iter(), pos);
        match selected {
          Some((&new_ship_id, new_ship)) => {
            if new_ship.owner.as_slice() == "player" {
              cmd::Follow(new_ship_id)
            } else {
              cmd::Attack(new_ship_id)
            }
          }
          None => {
            cmd::Goto(self.input_state.mouse_position)
          }
        }
      };

      for ship_id in self.selected_ships.iter() {
        self.universe.ships.find_mut(ship_id).with(|s| {
          s.command = Some(command);
        });
      }
    }
  }

  fn mouse_move(&mut self, _window: &mut W, args: &piston::MouseMoveArgs) {
    self.input_state.mouse_position = Vector2::new(args.x, args.y);
  }
}

fn ship_at_pos<'r, I: Iterator<(&'r uint, &'r Ship)>>(iter: I, pos: Vector2<f64>) -> Option<(&'r uint, &'r Ship)> {
  iter.filter_map(|(ship_id, ship)| {
    let dist2 = (ship.pos - pos).length2();
    if dist2 < SELECT_RADIUS*SELECT_RADIUS {
      Some((ship_id, ship, dist2))
    } else {
      None
    }
  })
    .min_by(|&(_, _, dist2)| dist2 as int)
    .map(|(ship_id, ship, _)| (ship_id, ship))
}

fn main() {
  let mut window = glfw_game_window::GameWindowGLFW::new(
    piston::GameWindowSettings {
      title: "Spaceships".to_string(),
      size: [800, 600],
      fullscreen: false,
      exit_on_esc: true
    }
  );

  let mut app = App::new();
  let game_iter_settings = piston::GameIteratorSettings {
    updates_per_second: 60,
    max_frames_per_second: 60
  };

  app.run(&mut window, &game_iter_settings);
}

trait With<T> {
  fn with(self, f: |T|);
}

impl<T> With<T> for Option<T> {
  fn with(self, f: |T|) {
    for x in self.move_iter() { f(x); }
  }
}

trait FindOrDefault<K,V> {
  fn find_or_default(&self, key: &K) -> V;
}

impl<K: Eq + Hash<S>, V: Default + Clone, S, H: Hasher<S>> FindOrDefault<K,V> for HashMap<K,V,H> {
  fn find_or_default(&self, key: &K) -> V {
    self.find_copy(key).unwrap_or_default()
  }
}

fn load_sound(filename: &str) -> al::Source {
  use std::slice::Vector;
  let mut stream = File::open(&Path::new(filename)).unwrap();
  let mut vf = vorbis::VorbisFile::new(stream).unwrap();
  let mut samples: Vec<i16> = Vec::new();
  loop {
    match vf.decode() {
      Ok(channels) => {
        samples.push_all_move(channels[0].iter().map(|x| (x * (i16::MAX - 1) as f32) as i16).collect());
      }
      _ => {
        break;
      }
    }
  }
  let buffer = al::Buffer::gen();
  let sample_freq: f32 = 44100.0;
  unsafe { buffer.buffer_data(al::FormatMono16, samples.as_slice(), sample_freq as al::ALsizei) };

  let source = al::Source::gen();
  source.queue_buffer(&buffer);
  return source;
}
