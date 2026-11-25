import {
  ifVar,
  map,
  rule,
  withCondition,
  type FromKeyParam,
  type ManipulatorBuilder,
  type ModifierParam,
  type ToKeyParam,
} from 'karabiner.ts'

const altLayer = 'kanata_alt'
const numpadLayer = 'kanata_numpad'
const caretNumpadLayer = 'kanata_caret_numpad'

const terminalBundleIdentifiers = [
  '^com\\.github\\.wez\\.wezterm$',
  '^com\\.apple\\.Terminal$',
  '^com\\.googlecode\\.iterm2$',
  '^net\\.kovidgoyal\\.kitty$',
  '^org\\.alacritty$',
  '^com\\.mitchellh\\.ghostty$',
  '^dev\\.warp\\.Warp(-Stable)?$',
]

const laptopCondition = withCondition(
  { type: 'device_if', identifiers: [{ is_built_in_keyboard: true }] },
  {
    type: 'frontmost_application_unless',
    bundle_identifiers: ['^com\\.apple\\.loginwindow$'],
  },
)

const externalCondition = withCondition(
  { type: 'device_unless', identifiers: [{ is_built_in_keyboard: true }] },
  {
    type: 'frontmost_application_unless',
    bundle_identifiers: ['^com\\.apple\\.loginwindow$'],
  },
)

const aptV3AngleMod = [
  // LHS
  ['q', 'w'], ['w', 'g'], ['e', 'd'], ['r', 'f'], ['t', 'b'],
  ['a', 'r'], ['s', 's'], ['d', 't'], ['f', 'h'], ['g', 'k'],
  ['z', 'c'], ['x', 'm'], ['c', 'p'], ['v', 'v'], ['b', 'x'],
  // RHS
  ['y', 'q'], ['u', 'l'], ['i', 'u'], ['o', 'o'], ['p', 'y'],
  ['h', 'j'], ['j', 'n'], ['k', 'e'], ['l', 'a'], [';', 'i'],
  ['n', 'z'], ['m', ';'], [',', ','], ['.', '.'], ['/', '/'],
] as const satisfies ReadonlyArray<readonly [FromKeyParam, ToKeyParam]>

const controlShortcutKeys = [
  'a', 'c', 'f', 'n', 'o', 'p', 's', 't', 'v', 'w', 'x', 'y', 'z',
] as const

const controlShortcutMap = new Map<ToKeyParam, ToKeyParam>(
  controlShortcutKeys.map(logicalKey => [
    logicalKey,
    logicalKey === 'y' ? 'z' : logicalKey,
  ]),
)

function controlShortcuts(
  physicalKey: FromKeyParam,
  logicalKey: ToKeyParam,
): ManipulatorBuilder[] {
  const commandKey = controlShortcutMap.get(logicalKey)
  if (!commandKey) {
    return []
  }

  const shortcuts = [
    map(physicalKey, ['control', 'shift']).to(commandKey, ['left_command', 'left_shift']),
    map(physicalKey, 'control').to(
      commandKey,
      logicalKey === 'y' ? ['left_command', 'left_shift'] : 'left_command',
    ),
  ]

  return [
    withCondition({
      type: 'frontmost_application_unless',
      bundle_identifiers: terminalBundleIdentifiers,
    })(shortcuts),
  ]
}

const symbolMapping: ReadonlyArray<readonly [FromKeyParam, ToKeyParam, ModifierParam?]> = [
  ['1', 'f1'], ['2', 'f2'], ['3', 'f3'], ['4', 'f4'], ['5', 'f5'],
  ['9', ',', 'left_shift'], ['0', '.', 'left_shift'],
  ['q', 'home'], ['w', 'page_up'], ['e', 'page_down'], ['r', 'end'],
  ['a', 'left_arrow'], ['s', 'up_arrow'], ['d', 'down_arrow'], ['f', 'right_arrow'],
  ['y', '5', 'left_shift'], ['u', '7', 'left_shift'], ['i', '-'],
  ['o', '8', 'left_shift'], ['p', '=', 'left_shift'],
  ['h', '1', 'left_shift'], ['j', 'tab'], ['k', 'delete_or_backspace'],
  ['l', '9', 'left_shift'], [';', '0', 'left_shift'],
  ['z', '2', 'left_shift'], ['x', 'grave_accent_and_tilde'], ['v', '3', 'left_shift'],
  ['b', 'grave_accent_and_tilde', 'left_shift'], ['n', '4', 'left_shift'], ['m', '-', 'left_shift'],
  [',', '='], ['.', '[', 'left_shift'], ['/', ']', 'left_shift'],
]

const numpadMapping: ReadonlyArray<readonly [FromKeyParam, ToKeyParam]> = [
  ['y', '='], ['u', '7'], ['i', '8'], ['o', '9'], ['p', '='],
  ['h', ','], ['j', '4'], ['k', '5'], ['l', '6'], [';', ','],
  ['n', '.'], ['m', '1'], [',', '2'], ['.', '3'], ['/', '.'],
  ['spacebar', '0'], ['return_or_enter', 'spacebar'],
]

export const karabinerRules = [
  rule('Laptop left Command - symbol/navigation layer').manipulators([
    laptopCondition([
      map('left_command', '', 'any')
        .toVar(altLayer, 1)
        .toAfterKeyUp([
          { set_variable: { name: altLayer, value: 0 } },
          { set_variable: { name: numpadLayer, value: 0 } },
          { set_variable: { name: caretNumpadLayer, value: 0 } },
        ]),
    ]),
  ]),

  rule('Laptop left Option - native Command').manipulators([
    laptopCondition([
      map('left_option', '', 'any').to('left_command'),
    ]),
  ]),

  rule('Laptop Command + Caps Lock - numpad layer').manipulators([
    laptopCondition([
      withCondition(ifVar(altLayer, 1))([
        map('caps_lock', '', 'any')
          .toVar(numpadLayer, 1)
          .toAfterKeyUp({ set_variable: { name: numpadLayer, value: 0 } }),
      ]),
    ]),
  ]),

  rule('Laptop Command + C - tap caret, hold numpad').manipulators([
    laptopCondition([
      withCondition(ifVar(altLayer, 1))([
        // Activate on key-down so a fast chord uses the numpad. A chord cancels the caret tap.
        map('c', '', 'any')
          .toVar(caretNumpadLayer, 1)
          .toIfAlone('6', 'left_shift')
          .toAfterKeyUp({ set_variable: { name: caretNumpadLayer, value: 0 } })
          .parameters({ 'basic.to_if_alone_timeout_milliseconds': 150 }),
      ]),
    ]),
  ]),

  rule('Laptop numpad').manipulators([
    laptopCondition([
      // Each trigger owns its state so releasing one leaves the other active.
      ...[numpadLayer, caretNumpadLayer].flatMap(triggerLayer =>
        withCondition(ifVar(altLayer, 1), ifVar(triggerLayer, 1))(
          numpadMapping.map(([physicalKey, outputKey]) =>
            map(physicalKey, '', 'any').to(outputKey),
          ),
        ),
      ),
    ]),
  ]),

  rule('Laptop symbols and navigation').manipulators([
    laptopCondition([
      withCondition(ifVar(altLayer, 1))([
        map('s', 'shift', 'any').to('s', ['left_option', 'left_shift']),
        map('d', 'shift', 'any').to('t', ['left_option', 'left_shift']),
        ...symbolMapping.map(([physicalKey, outputKey, modifiers]) =>
          map(physicalKey, '', 'any').to(outputKey, modifiers),
        ),
        map('tab', '', 'any').to('tab', 'left_command'),
      ]),
    ]),
  ]),

  rule('Laptop Caps Lock - tap Escape, hold Control').manipulators([
    laptopCondition([
      map('caps_lock', '', 'any')
        .to('left_control')
        .toIfAlone('escape')
        .parameters({ 'basic.to_if_alone_timeout_milliseconds': 150 }),
    ]),
  ]),

  rule('Laptop Angle layout and Windows Control shortcuts').manipulators([
    laptopCondition(
      aptV3AngleMod.flatMap(([physicalKey, logicalKey]) => [
        ...controlShortcuts(physicalKey, logicalKey),
        map(physicalKey, '', 'any').to(logicalKey),
      ]),
    ),
  ]),

  rule('External keyboard Windows Control shortcuts').manipulators([
    externalCondition(
      controlShortcutKeys.flatMap(logicalKey =>
        controlShortcuts(logicalKey, logicalKey),
      ),
    ),
  ]),
]
