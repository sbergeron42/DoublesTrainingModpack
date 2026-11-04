# Doubles Training Modpack

A fork of the [Ultimate Training Modpack](https://github.com/jugeeya/UltimateTrainingModpack) that turns Smash
Ultimate's Training mode into a doubles lab: up to four fighters, any mix of humans and up to 3 CPUs.

**[Latest release](https://github.com/sbergeron42/DoublesTrainingModpack/releases/latest)**
(`libdubs_training_modpack.nro`). See [Installation](#installation).

<p align="center">
<img src="./photos/doubles/doubles-match.gif" width="640">
</p>

<p align="center"><i>Red: Captain Falcon (P1, human) and Zelda (CPU, Lv. 9). Blue: Donkey Kong
(P2, human) and Wolf (CPU, Lv. 1).</i></p>

<a name="doubles-features"/>

# Doubles features

## Team Training CSS
<p align="center">
<img src="./photos/doubles/team-css.jpg" width="640">
</p>

<p align="center"><i>`X` to toggle between Training and Team Training. UI works like in Smash mode's Team Battle.</i></p>

## In game

- **Doubles-specific mechanics:** Teammates don't push or footstool each other.
- **Team Attack** is on by default. Turn it off in the modpack menu.
- **Persisted unique CPU difficulty levels:** a Lv. 9 Zelda and a Lv. 1 Wolf play at Lv. 9 and Lv. 1.

## Vanilla Training Menu Options

<p align="center">
<img src="./photos/doubles/per-player-menu.gif" width="640">
</p>

- **Vanilla training menu (`+`):** select a row and tap `L` / `R` to switch fighter.
  - **Player Damage %** sets P1 to P4. It replaces CPU Damage % and P1 Damage % in vanilla Ultimate.
  - **CPU Behavior** is set per CPU, with same options as vanilla: 'Stop', 'Walk', 'Lvl (n)', etc.

## Modpack Menu Options

Hold `D-Pad Up` and press `B`, or hold `Start`. Reset is `Y` + `R` (`Z` on GameCube).

### CPU Profiles

<p align="center">
<img src="./photos/doubles/cpu-profiles.gif" width="640">
</p>

<p align="center"><i>Up to the title, then Left / Right to change profile.
<code>L</code> / <code>R</code> switch CPU (<code>Y</code> + <code>L</code> / <code>R</code> on GameCube).</i></p>

Each CPU uses one of eight profiles (Mash, Override and Defensive tabs).

### Doubles Settings

<p align="center">
<img src="./photos/doubles/doubles-settings.jpg" width="640">
</p>

# Installation

You need [Atmosphere](https://github.com/Atmosphere-NX/Atmosphere/releases) and
[Skyline](https://github.com/skyline-dev/skyline/releases), as for the regular Training Modpack.

1. Put Skyline's `exefs` folder in `sd:/atmosphere/contents/01006A800016E000/`.
2. Put these in `sd:/atmosphere/contents/01006A800016E000/romfs/skyline/plugins/`:
   - `libdubs_training_modpack.nro` ([releases](https://github.com/sbergeron42/DoublesTrainingModpack/releases/latest));
   - `libnro_hook.nro` ([nro-hook-plugin](https://github.com/ultimate-research/nro-hook-plugin/releases));
   - `libparam_hook.nro` ([params-hook-plugin](https://github.com/ultimate-research/params-hook-plugin/releases)).
3. **Remove `libtraining_modpack.nro` if installed.** Running both isn't yet supported.

Settings are saved to `sd:/ultimate/TrainingModpack/training_modpack_dubs.conf`.

<a name="compatibility"/>

# Compatibility/Limitations

- **Tested on Smash Ultimate 13.0.5.**
- Not yet able to co-exist with Jugeeya's Ultimate Training Modpack. WIP.
- Some features haven't been tested/scaled to 4 players, like character items.
- Experimental/doubles-native features TBD (open to feedback!)

<a name="features"/>

# Training Modpack features

Everything from the Ultimate Training Modpack is included. Its
[README](https://github.com/jugeeya/UltimateTrainingModpack#readme) has screenshots, every option and the FAQ.

- **Mash, follow-up and defensive options** (set per CPU here): triggers, aerial delay, fast fall, DI, SDI,
  air dodge, ledge, tech, shield and more.
- **Save states** (default `ZL` + `D-Pad Down` to save, `ZL` + `D-Pad Up` to load):
  - five slots, mirroring and randomized damage;
  - charge levels, buffs and character items.
- **Input recording** (`ZR` + `D-Pad Down` to record, `ZR` + `D-Pad Up` to play back), usable as a CPU option.
- **Misc:** hitbox visualization, frame advantage, input delay, stage hazards on/off, HUD toggle.
- **Menu use:** when several options are picked, one is chosen at random. `X` saves the current settings as your
  defaults.

<a name="build"/>

# Build from source

Needs nightly Rust (see `rust-toolchain.toml`) and [cargo-skyline](https://github.com/jam1garner/cargo-skyline).

```bash
cargo skyline build --release
```

- **Output:** `target/aarch64-skyline-switch/release/libdubs_training_modpack.nro`.
- **`doubles_debug_log`:** writes a log to `sd:/ultimate/TrainingModpack/doubles_debug.log`.
- **`doubles_ui_diagnostics`:** adds UI pane probes and also turns on the log. Keep it off in emulators, where
  it can stall.

<a name="credits"/>

# Credits

Built on the [Ultimate Training Modpack](https://github.com/jugeeya/UltimateTrainingModpack) by jugeeya and
[its contributors](https://github.com/jugeeya/UltimateTrainingModpack/graphs/contributors), who made the menus,
save states and CPU options this mod builds on.
