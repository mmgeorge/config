import assert from 'node:assert/strict'
import { test } from 'node:test'
import { complexModifications, type BasicManipulator } from 'karabiner.ts'
import { karabinerRules } from './config'

const modifications = complexModifications(karabinerRules)
const physicalKeys = 'q w e r t y u i o p a s d f g h j k l ; z x c v b n m , . /'.split(' ')
const logicalKeys = 'w g d f b q l u o y r s t h k j n e a i c m p v x z ; , . /'.split(' ')

function assertJsonEqual(actual: unknown, expected: unknown) {
  assert.deepEqual(JSON.parse(JSON.stringify(actual)), expected)
}

function mapping(description: string, key: string, mandatory: string[] = []): BasicManipulator {
  const selectedRule = modifications.rules.find(candidate => candidate.description === description)
  assert.ok(selectedRule, `Missing rule: ${description}`)
  const selected = selectedRule.manipulators.find(candidate =>
    candidate.type === 'basic'
    && 'key_code' in candidate.from
    && candidate.from.key_code === key
    && JSON.stringify([...(candidate.from.modifiers?.mandatory ?? [])].sort())
      === JSON.stringify([...mandatory].sort()),
  )
  assert.ok(selected && selected.type === 'basic', `Missing mapping: ${description}, ${key}, ${mandatory}`)
  return selected
}

test('Angle layout matches Kanata while preserving unhandled modifiers', () => {
  for (const [index, physicalKey] of physicalKeys.entries()) {
    const selected = mapping(
      'Laptop Angle layout and Windows Control shortcuts',
      physicalKey === ';' ? 'semicolon'
        : physicalKey === ',' ? 'comma'
        : physicalKey === '.' ? 'period'
        : physicalKey === '/' ? 'slash' : physicalKey,
    )
    const logicalKey = logicalKeys[index] === ';' ? 'semicolon'
      : logicalKeys[index] === ',' ? 'comma'
      : logicalKeys[index] === '.' ? 'period'
      : logicalKeys[index] === '/' ? 'slash' : logicalKeys[index]
    assertJsonEqual(selected.to, [{ key_code: logicalKey }])
    assertJsonEqual(selected.from.modifiers, { optional: ['any'] })
  }
})

test('Control shortcuts use logical letters on the laptop and native letters externally', () => {
  const shortcutKeys = ['a', 'c', 'f', 'n', 'o', 'p', 's', 't', 'v', 'w', 'x', 'y', 'z']
  for (const logicalKey of shortcutKeys) {
    const physicalKey = physicalKeys[logicalKeys.indexOf(logicalKey)]
    assert.ok(physicalKey)
    for (const [description, key] of [
      ['Laptop Angle layout and Windows Control shortcuts', physicalKey],
      ['External keyboard Windows Control shortcuts', logicalKey],
    ]) {
      const selected = mapping(description, key, ['control'])
      assertJsonEqual(selected.to, [{
        key_code: logicalKey === 'y' ? 'z' : logicalKey,
        modifiers: logicalKey === 'y' ? ['left_command', 'left_shift'] : ['left_command'],
      }])
      assertJsonEqual(
        mapping(description, key, ['control', 'shift']).to,
        [{
          key_code: logicalKey === 'y' ? 'z' : logicalKey,
          modifiers: ['left_command', 'left_shift'],
        }],
      )
    }
  }
})

test('All terminal Control shortcuts remain native on both keyboard types', () => {
  const terminalIdentifiers = [
    'com.github.wez.wezterm',
    'com.apple.Terminal',
    'com.googlecode.iterm2',
    'net.kovidgoyal.kitty',
    'org.alacritty',
    'com.mitchellh.ghostty',
    'dev.warp.Warp-Stable',
  ]
  const shortcutKeys = ['a', 'c', 'f', 'n', 'o', 'p', 's', 't', 'v', 'w', 'x', 'y', 'z']
  for (const logicalKey of shortcutKeys) {
    const physicalKey = physicalKeys[logicalKeys.indexOf(logicalKey)]
    assert.ok(physicalKey)
    for (const [description, key] of [
      ['Laptop Angle layout and Windows Control shortcuts', physicalKey],
      ['External keyboard Windows Control shortcuts', logicalKey],
    ]) {
      for (const modifiers of [['control'], ['control', 'shift']]) {
        const selected = mapping(description, key, modifiers)
        for (const bundleIdentifier of [...terminalIdentifiers, 'md.obsidian', 'com.apple.Safari']) {
          const excluded = selected.conditions?.some(condition =>
            condition.type === 'frontmost_application_unless'
            && 'bundle_identifiers' in condition
            && condition.bundle_identifiers?.some(pattern => new RegExp(pattern).test(bundleIdentifier)),
          ) ?? false
          assert.equal(
            excluded,
            terminalIdentifiers.includes(bundleIdentifier),
            `${description}: ${modifiers}+${logicalKey} in ${bundleIdentifier}`,
          )
        }
      }
    }
  }
})

test('Laptop-only layers and shortcuts retain device and login boundaries', () => {
  for (const selectedRule of modifications.rules) {
    for (const selected of selectedRule.manipulators) {
      const conditions = selected.conditions ?? []
      assert.ok(conditions.some(condition =>
        condition.type === (selectedRule.description.startsWith('External') ? 'device_unless' : 'device_if')
        && 'identifiers' in condition
        && condition.identifiers.some(identifier => identifier.is_built_in_keyboard === true),
      ))
      assert.ok(conditions.some(condition =>
        condition.type === 'frontmost_application_unless'
        && 'bundle_identifiers' in condition
        && condition.bundle_identifiers?.includes('^com\\.apple\\.loginwindow$'),
      ))
      assert.ok(
        selected.type === 'basic'
        && 'key_code' in selected.from
        && !['left_control', 'right_control', 'right_command', 'right_option', 'delete_forward']
          .includes(String(selected.from.key_code)),
      )
    }
  }
})

test('Left Option provides native Command chords without activating the symbol layer', () => {
  const selected = mapping('Laptop left Option - native Command', 'left_option')
  assertJsonEqual(selected.from.modifiers, { optional: ['any'] })
  assertJsonEqual(selected.to, [{ key_code: 'left_command' }])
  assert.equal(selected.to_if_alone, undefined)
  assert.equal(selected.to_if_held_down, undefined)
  assert.equal(selected.to_after_key_up, undefined)
  const optionMappings = modifications.rules.flatMap(selectedRule =>
    selectedRule.manipulators.filter(candidate =>
      candidate.type === 'basic'
      && 'key_code' in candidate.from
      && candidate.from.key_code === 'left_option',
    ),
  )
  assert.deepEqual(optionMappings, [selected])
})

test('Command and Caps Lock set held layers and release their state', () => {
  const command = mapping('Laptop left Command - symbol/navigation layer', 'left_command')
  assertJsonEqual(command.from.modifiers, { optional: ['any'] })
  assertJsonEqual(command.to, [{ set_variable: { name: 'kanata_alt', value: 1 } }])
  assertJsonEqual(command.to_after_key_up, [
    { set_variable: { name: 'kanata_alt', value: 0 } },
    { set_variable: { name: 'kanata_numpad', value: 0 } },
  ])
  const numpad = mapping('Laptop Command + Caps Lock - numpad layer', 'caps_lock')
  assertJsonEqual(numpad.to, [{ set_variable: { name: 'kanata_numpad', value: 1 } }])
  assertJsonEqual(numpad.to_after_key_up, [
    { set_variable: { name: 'kanata_numpad', value: 0 } },
  ])
  assert.ok(numpad.conditions?.some(condition =>
    condition.type === 'variable_if' && condition.name === 'kanata_alt' && condition.value === 1,
  ))
})

test('Symbol and navigation outputs match Kanata, with macOS application switching', () => {
  const expected: Array<[string, string, string?]> = [
    ['q', 'vk_none'], ['w', 'page_up'], ['e', 'page_down'],
    ['r', 'grave_accent_and_tilde', 'left_shift'], ['t', 'vk_none'],
    ['y', '5', 'left_shift'], ['u', '7', 'left_shift'], ['i', 'hyphen'],
    ['o', '8', 'left_shift'], ['p', 'equal_sign', 'left_shift'],
    ['a', 'left_arrow'], ['s', 'up_arrow'], ['d', 'down_arrow'], ['f', 'right_arrow'],
    ['g', 'vk_none'], ['h', '1', 'left_shift'], ['j', 'tab'], ['k', 'delete_or_backspace'],
    ['l', '9', 'left_shift'], ['semicolon', '0', 'left_shift'],
    ['z', '2', 'left_shift'], ['x', '3', 'left_shift'],
    ['c', 'grave_accent_and_tilde'], ['v', 'grave_accent_and_tilde'],
    ['b', '6', 'left_shift'], ['n', '4', 'left_shift'], ['m', 'hyphen', 'left_shift'],
    ['comma', 'equal_sign'], ['period', 'open_bracket', 'left_shift'],
    ['slash', 'close_bracket', 'left_shift'], ['tab', 'tab', 'left_command'],
  ]
  for (const [physicalKey, outputKey, modifier] of expected) {
    const selected = mapping('Laptop symbols and navigation', physicalKey)
    assertJsonEqual(
      selected.to,
      [{ key_code: outputKey, ...(modifier ? { modifiers: [modifier] } : {}) }],
    )
    assert.ok(selected.conditions?.some(condition =>
      condition.type === 'variable_if' && condition.name === 'kanata_alt' && condition.value === 1,
    ))
  }
  for (const [physicalKey, outputKey] of [['s', 's'], ['d', 't']]) {
    assertJsonEqual(
      mapping('Laptop symbols and navigation', physicalKey, ['shift']).to,
      [{ key_code: outputKey, modifiers: ['left_option', 'left_shift'] }],
    )
  }
})

test('Numpad overrides symbols only while both layer keys are held', () => {
  const expected: Array<[string, string, string?]> = [
    ['y', 'equal_sign'], ['u', '7'], ['i', '8'], ['o', '9'], ['p', 'hyphen'],
    ['h', 'comma'], ['j', '4'], ['k', '5'], ['l', '6'], ['semicolon', 'equal_sign', 'left_shift'],
    ['n', 'period'], ['m', '1'], ['comma', '2'], ['period', '3'], ['slash', 'slash'],
    ['spacebar', '0'],
  ]
  for (const [physicalKey, outputKey, modifier] of expected) {
    const selected = mapping('Laptop numpad', physicalKey)
    assertJsonEqual(
      selected.to,
      [{ key_code: outputKey, ...(modifier ? { modifiers: [modifier] } : {}) }],
    )
    assertJsonEqual(
      selected.conditions?.filter(condition => condition.type === 'variable_if'),
      [
        { type: 'variable_if', name: 'kanata_alt', value: 1 },
        { type: 'variable_if', name: 'kanata_numpad', value: 1 },
      ],
    )
  }
  const descriptions = modifications.rules.map(selectedRule => selectedRule.description)
  assert.ok(descriptions.indexOf('Laptop numpad') < descriptions.indexOf('Laptop symbols and navigation'))
  assert.ok(
    descriptions.indexOf('Laptop symbols and navigation')
    < descriptions.indexOf('Laptop Angle layout and Windows Control shortcuts'),
  )
})

test('Caps Lock acts as immediate Control for chords and Escape for a tap', () => {
  const selected = mapping('Laptop Caps Lock - tap Escape, hold Control', 'caps_lock')
  assertJsonEqual(selected.to, [{ key_code: 'left_control' }])
  assertJsonEqual(selected.to_if_alone, [{ key_code: 'escape' }])
  assertJsonEqual(selected.parameters, { 'basic.to_if_alone_timeout_milliseconds': 150 })
})
