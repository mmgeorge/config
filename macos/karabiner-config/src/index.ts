import {
  map,
  rule,
  ToKeyCode,
  withCondition,
  withMapper,
  withModifier,
  writeToProfile,
} from 'karabiner.ts'

const aptV3AngleMod = {
  // LHS
  "q": "w",
  "w": "g",
  "e": "d",
  "r": "f",
  "t": "b",

  "a": "r",
  "s": "s",
  "d": "t",
  "f": "h",
  "g": "k",

  "z": "c",
  "x": "m",
  "c": "p",
  "v": "v",
  "b": "x",

  // RHS
  "y": "q",
  "u": "l",
  "i": "u",
  "o": "o",
  "p": "y",

  "h": "j",
  "j": "n",
  "k": "e",
  "l": "a",
  ";": "i",

  "n": "z",
  "m": ";",
  // ".": ".",
  // ",": ",",
  // "/": "/",
}

writeToProfile('Default profile', [
  rule("control - as command").manipulators([
    withCondition(
      { type: "device_if", identifiers: [{ is_built_in_keyboard: false }] },
      { type: "frontmost_application_unless", bundle_identifiers: ["^com\\.github\\.wez\\.wezterm$"] }
    )(
      [
        map('left_control').to('left_command'),
      ])
  ]),

  rule("laptop option - as command").manipulators([
    withCondition(
      { type: "device_if", identifiers: [{ is_built_in_keyboard: true }] },
      // { type: "frontmost_application_unless", bundle_identifiers: ["^com\\.github\\.wez\\.wezterm$"] }
    )(
      [
        map('left_option').to('right_command'),
      ])
  ]),

  rule("caps-lock - as command").manipulators([
    withCondition(
      { type: "device_if", identifiers: [{ is_built_in_keyboard: true }] },
      { type: "frontmost_application_unless", bundle_identifiers: ["^com\\.github\\.wez\\.wezterm$"] }
    )(
      [
        map('caps_lock').to('right_command'),
      ])
  ]),

  rule("caps-lock - as escape-control").manipulators([
    withCondition(
      { type: "device_if", identifiers: [{ is_built_in_keyboard: true }] },
      { type: "frontmost_application_if", bundle_identifiers: ["^com\\.github\\.wez\\.wezterm$"] }

    )([
      map('caps_lock').toIfHeldDown('left_control').toIfAlone('escape')
    ])
  ]),

  rule("delete").manipulators([
    map('delete_forward').to('delete_or_backspace', 'left_command')
  ]),

  rule("left-command - layer").manipulators([
    withCondition(
      { type: "device_if", identifiers: [{ is_built_in_keyboard: true }] },
      { type: "frontmost_application_unless", bundle_identifiers: ["^com\\.apple\\.loginwindow$"] }
    )([
      withModifier('left_command')([
        // map('q').to('w'),
        map('q').to('home'),
        map('w').to('page_up'),
        map('e').to('page_down'),
        map('r').to('end'), // ~
        // map('r').to('grave_accent_and_tilde', 'left_shift'), // ~
        map('t').to('b'),

        map('a').to('left_arrow'),
        map('s').to('up_arrow'),
        map('d').to('down_arrow'),
        map('f').to('right_arrow'),
        map('g').to('k'),

        map('z').to('6', 'left_shift'), // ^
        map('x').to('2', 'left_shift'), // @
        map('c').to('3', 'left_shift'), // #
        map('v').to('grave_accent_and_tilde', 'left_shift'), // `
        map('b').to('x'),

        // RHS
        map('y').to('5', 'left_shift'), // %
        map('u').to('7', 'left_shift'), // &
        map('i').to('-'),
        map('o').to('8', 'left_shift'), // *
        map('p').to('=', 'left_shift'), // +

        map('h').to('1', 'left_shift'),
        map('j').to('tab'),
        map('k').to('delete_or_backspace'),
        map('l').to('9', 'left_shift'), // (
        map(';').to('0', 'left_shift'), // )

        map('n').to('4', 'left_shift'), // $
        map('m').to('-', 'left_shift'), // _
        map('.').to('='),
        map(',').to('[', 'left_shift'), // {
        map('/').to(']', 'left_shift'), // }
      ])
    ])
  ]),

  rule('Key mapping').manipulators([
    withCondition(
      { type: "device_if", identifiers: [{ is_built_in_keyboard: true }] },
      { type: "frontmost_application_unless", bundle_identifiers: ["^com\\.apple\\.loginwindow$"] }
    )([
      withMapper(aptV3AngleMod)((k, v) => map(k).to(v as ToKeyCode)),
      withMapper(aptV3AngleMod)((k, v) => map(k, "shift").to(v as ToKeyCode, "shift")),
      withMapper(aptV3AngleMod)((k, v) => map(k, "right_command").to(v as ToKeyCode, "right_command")),
      withMapper(aptV3AngleMod)((k, v) => map(k, "control").to(v as ToKeyCode, "right_control")),
      withMapper(aptV3AngleMod)((k, v) => map(k, "option").to(v as ToKeyCode, "option"))
    ])
  ])
])
