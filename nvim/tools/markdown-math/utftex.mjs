#!/usr/bin/env node

import path from 'node:path'
import { pathToFileURL } from 'node:url'

let input = ''
process.stdin.setEncoding('utf8')
for await (const chunk of process.stdin) input += chunk

const packageDirectory = path.dirname(process.argv[1])
const modulePath = path.resolve(packageDirectory, '..', 'libtexprintf', 'libtexprintf.js')
const { createRender, loadInstance } = await import(pathToFileURL(modulePath).href)
const render = createRender(await loadInstance())
const result = render(input)
if (!result.output) {
  process.stderr.write(`${result.errors.join('\n') || 'utftex produced no output'}\n`)
  process.exit(1)
}
process.stdout.write(result.output)
