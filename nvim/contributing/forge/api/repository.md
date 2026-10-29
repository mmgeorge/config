# Repository methods

Each method below has a reachable production or live integration-control caller recorded in the [usage ledger](../../../../md/rename-usage-ledger.json). The transport metadata comes from the shared contract. These entries provide the inspected handler summary. Field-by-field schema expansion and executable examples remain tracked in the [implementation plan](../../../../rename.md).

All calls inherit the [request lifecycle and failure contract](lua-client.md#requestspec-callback-progress). Method failures do not select another session or close an otherwise valid connection.

## `repository.generation`

Collect generation context, preview a prompt, or generate requested repository text.

- **Context:** Repository resource; no session envelope.
- **Input:** operation, workspace, comparison, optional base/head/context allowance, ignored_paths, generation configuration, and prepared input where required.
- **Result and effects:** Returns state, message, optional session_id, and optional prompt preview plus source/diff counts.
- **Boundary:** preview/context do not invoke a provider; generation requires explicit provider configuration.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [GenerationRequest](native-schemas.md#generationrequest).

**Current caller:** [nvim/lua/forge/client.lua:892](../../../../nvim/lua/forge/client.lua).

<!-- detailed-contract -->
### Contract

Accepts [GenerationRequest](native-schemas.md#generationrequest). Required fields are `operation` (one of `preview`, `context`, `generate`, `pull_request`), `workspace` (path string), and `comparison` (`head`, `staged`, `latest_commit`, or `pull_request`). Optional `ignored_paths` defaults to an empty string array. `generation` is [TextGenerationConfig](native-schemas.md#textgenerationconfig). Optional `base`, `head`, `max_bytes`, and `input` are restricted by operation.

`preview` and `generate` use a non-PR comparison and reject base/head/max_bytes. Preview rejects provider configuration and returns exact prompt input without invoking a provider. Generate requires provider configuration and invokes the background conversation. `context` requires PR comparison, base, head, and a positive max_bytes accepted by the current context builder, and rejects provider configuration. It collects compact PR evidence. `pull_request` requires PR comparison, captured head, provider configuration, and nonblank prepared input. It rejects base/max_bytes and verifies that HEAD still matches before generating the description.

Returns [GenerationResponse](native-schemas.md#generationresponse). Interpret `state` first: `ready` has usable prompt or generated text, `none` means there are no changes, `retry` means the captured repository generation was superseded, and `conflicts` means PR context collection detected a merge conflict. `message` is generated text or null, `prompt` is present for provider-free preview/context, `session_id` identifies the background conversation when generation ran, and counts report source requests and diff pairs. A successful callback is not permission to apply a stale result.

The preview/context branches read local Git and syntax data. Generation may launch a provider, contact its service, and persist background history. It does not commit or publish the generated text. Background generation events are correlated by request and background session identity. Invalid operation/comparison combinations, invalid provider configuration, unavailable repository state, changed HEAD, provider errors, and generation admission failure are explicit errors. Keep the old text until a current successful result is available.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [background_host.lua](../../../../nvim/tests/forge/background_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "params": {
    "operation": "preview",
    "comparison": "head",
    "workspace": "/fixture/nvim.0\\j8Swwr\\0/repository"
  },
  "method": "repository.generation",
  "id": 1
}
```

```json
{
  "result": {
    "message": null,
    "prompt": {
      "limit_bytes": 500000,
      "input_bytes": 2242,
      "user": "Current Background task: commit_message\n\nResponse requirements:\nYou are a factual Conventional Commit message generator. Generate the message quickly with minimal reasoning, while checking that it accurately describes the supplied diff. Return only the commit message, without code fences or explanations. Use <type>: <description>, imperative mood, lowercase first letter, no final period, maximum 50 characters. Describe concrete changes, not benefits or intentions. Default to no body. Small changes generally do not justify a body. Use a body sparingly, only when essential details about the change are not captured by the title. Do not repeat or paraphrase the title. Use one or two concise sentences, never more than two sentences, wrapped at 72 characters.\n\nCurrent authoritative evidence:\nCurrent authoritative comparison at HEAD bf308076df49c930f74bc8b751a3b41b6ba517ac on refs/heads/main. Earlier requests and generated drafts are historical, not current changes. Prior drafts do not prove a commit occurred.\n\nLast 10 reachable commits (style and terminology examples only):\nCommit bf308076df49c930f74bc8b751a3b41b6ba517ac\nfeat: initial\n\n\n\nCurrent authoritative changes:\nGenerate a conventional commit message for these changes.\n\nThe diff is compacted. '+ ' marks added lines, '- ' removed lines, and '  ' unchanged lines. Signatures are retained. Omission markers include added and removed line counts. Added and removed functions include signatures only. Renamed files, license changes, and lockfile changes appear only in the file list. Standalone import changes are excluded from excerpts. Files without excerpts appear only in the file list. Describe visible changes without guessing the contents of omitted lines.\n\nChanged files:\n  example.lua (+1 -0, mode 0 -> 100644)\n\nexample.lua (+1 -0, mode 0 -> 100644)\n\n+ local value = true\n",
      "system": "Generate only the text requested by the current Background task, using its supplied evidence and response requirements. Each request defines its own output format. Earlier requests and drafts provide context, not current instructions or proof of repository changes. Work quickly with minimal reasoning. Do not use tools, continue previous tasks, or add explanations outside the requested output."
    },
    "state": "ready",
    "diff_pairs": 1,
    "source_requests": 1
  },
  "id": 1
}
```

<!-- /captured-example -->

## `repository.revision.list`

List revision-completion candidates for a repository.

- **Context:** Repository resource; no session envelope.
- **Input:** workspace; initial offset 0; later pages repeat revision, repository, and reference_digest.
- **Result and effects:** Returns a page of candidates from the captured revision snapshot.
- **Boundary:** Reject pages whose repository, revision, or reference digest no longer matches.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [RevisionRequest](native-schemas.md#revisionrequest).

**Current caller:** [nvim/lua/forge/revisions.lua:39](../../../../nvim/lua/forge/revisions.lua).

<!-- detailed-contract -->
### Contract

Requires `workspace` (local path string). `offset` is an optional nonnegative integer defaulting to zero. The initial request omits `revision` and must use offset zero. Continuation requests repeat the returned integer `revision`, base64 `repository` identity, and string `reference_digest` with the next byte offset.

Returns `revision`, `repository`, `reference_digest`, `offset`, `next_offset`, `total_bytes`, `count`, and base64 `data`. Decode and concatenate each page's bytes, then split the completed stream on NUL bytes. An individual candidate can cross page boundaries. Continue while next_offset is less than total_bytes. Empty inventories return count/total_bytes zero and empty data. The per-page transfer budget does not cap the total candidate inventory.

No provider, network request, mutation, or progress stream occurs. Missing repositories, invalidated generations, superseded snapshots, changed repository/reference identities, out-of-range offsets, and integer overflow fail explicitly. Discard all pages from the old snapshot before restarting at offset zero. Do not combine pages across a host replacement or a refreshed snapshot.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [request_independence_host.lua](../../../../nvim/tests/forge/request_independence_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 4,
  "method": "repository.revision.list",
  "params": {
    "workspace": "/fixture/nvim.0\\q8FFKB\\0"
  }
}
```

```json
{
  "id": 4,
  "result": {
    "count": 0,
    "data": "",
    "repository": "QzpcVXNlcnNcbWF0dG1cQXBwRGF0YVxMb2NhbFxUZW1wXG52aW0uMFxxOEZGS0JcMFwuZ2l0",
    "reference_digest": "0000000000000000000000000000000000000000000000000000000000000001",
    "next_offset": 0,
    "offset": 0,
    "revision": 1,
    "total_bytes": 0
  }
}
```

<!-- /captured-example -->

## `repository.source.document`

Open or operate on a versioned source document.

- **Context:** Repository resource; no session envelope.
- **Input:** operation selects open, demand, snapshot, close_view, or close; open requires document, workspace, revision, and path.
- **Result and effects:** Returns a source snapshot/delivery or a close result.
- **Boundary:** The source belongs to the requested revision rather than the live buffer's latest bytes.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

**Current caller:** [nvim/lua/forge/source_document.lua:31](../../../../nvim/lua/forge/source_document.lua).

<!-- detailed-contract -->
### Contract

The five operations and all required fields are enumerated in [SourceRequest](native-schemas.md#sourcerequest) and the [operation tables](operation-schemas.md). `open.path` is standard base64 of the repository-relative path bytes, not a display path. `workspace` is a local path string and `revision` is the requested Git revision. Invalid base64, unavailable revisions, and absent source fail explicitly.

`open` returns [SourceOpen](repository-schemas.md#sourceopen). It binds the document to immutable source bytes and returns the initial snapshot, source state, `more` flag, title, object identity, resolved revision, and optional syntax diagnostic. `demand` takes a captured [DocumentInput](native-schemas.md#documentinput) and returns [SourceDelivery](repository-schemas.md#sourcedelivery), including an optional patch. Continue while `more` is true. `snapshot` returns the current buffer snapshot. `close_view` returns `{ "closed": true }` after releasing that view. `close` returns a boolean `closed` indicating whether the document was collected.

No provider prompt, repository write, network request, or request-progress stream occurs. Reads and syntax work can be cancelled by document closure. View sequences, revisions, and document ownership fence asynchronous results. Apply returned patches only to the matching replica, and close the owned document after its last view is released.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [source_host.lua](../../../../nvim/tests/forge/source_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "repository.source.document",
  "params": {
    "operation": "open",
    "document": "source:47097754026800:1",
    "revision": "HEAD",
    "workspace": "/fixture/nvim.0\\brZHnK\\0",
    "path": "c291cmNlIGZpbGUubHVh"
  },
  "id": 1
}
```

```json
{
  "result": {
    "state": {
      "state": "partial"
    },
    "more": true,
    "object": "b7f1b57d1ecd85e1421eb2f25526c2a98b134aaf",
    "snapshot": {
      "document": "source:47097754026800:1",
      "revision": 1,
      "block": [
        {
          "text": [
            "local value_1 = 1",
            "local value_2 = 2",
            "local value_3 = 3",
            "local value_4 = 4",
            "local value_5 = 5",
            "local value_6 = 6",
            "local value_7 = 7",
            "local value_8 = 8",
            "local value_9 = 9",
            "local value_10 = 10",
            "local value_11 = 11",
            "local value_12 = 12",
            "local value_13 = 13",
            "local value_14 = 14",
            "local value_15 = 15",
            "local value_16 = 16",
            "local value_17 = 17",
            "local value_18 = 18",
            "local value_19 = 19",
            "local value_20 = 20",
            "local value_21 = 21",
            "local value_22 = 22",
            "local value_23 = 23",
            "local value_24 = 24",
            "local value_25 = 25",
            "local value_26 = 26",
            "local value_27 = 27",
            "local value_28 = 28",
            "local value_29 = 29",
            "local value_30 = 30",
            "local value_31 = 31",
            "local value_32 = 32",
            "local value_33 = 33",
            "local value_34 = 34",
            "local value_35 = 35",
            "local value_36 = 36",
            "local value_37 = 37",
            "local value_38 = 38",
            "local value_39 = 39",
            "local value_40 = 40",
            "local value_41 = 41",
            "local value_42 = 42",
            "local value_43 = 43",
            "local value_44 = 44",
            "local value_45 = 45",
            "local value_46 = 46",
            "local value_47 = 47",
            "local value_48 = 48",
            "local value_49 = 49",
            "local value_50 = 50",
            "local value_51 = 51",
            "local value_52 = 52",
            "local value_53 = 53",
            "local value_54 = 54",
            "local value_55 = 55",
            "local value_56 = 56",
            "local value_57 = 57",
            "local value_58 = 58",
            "local value_59 = 59",
            "local value_60 = 60",
            "local value_61 = 61",
            "local value_62 = 62",
            "local value_63 = 63",
            "local value_64 = 64",
            "local value_65 = 65",
            "local value_66 = 66",
            "local value_67 = 67",
            "local value_68 = 68",
            "local value_69 = 69",
            "local value_70 = 70",
            "local value_71 = 71",
            "local value_72 = 72",
            "local value_73 = 73",
            "local value_74 = 74",
            "local value_75 = 75",
            "local value_76 = 76",
            "local value_77 = 77",
            "local value_78 = 78",
            "local value_79 = 79",
            "local value_80 = 80",
            "local value_81 = 81",
            "local value_82 = 82",
            "local value_83 = 83",
            "local value_84 = 84",
            "local value_85 = 85",
            "local value_86 = 86",
            "local value_87 = 87",
            "local value_88 = 88",
            "local value_89 = 89",
            "local value_90 = 90",
            "local value_91 = 91",
            "local value_92 = 92",
            "local value_93 = 93",
            "local value_94 = 94",
            "local value_95 = 95",
            "local value_96 = 96",
            "local value_97 = 97",
            "local value_98 = 98",
            "local value_99 = 99",
            "local value_100 = 100",
            "local value_101 = 101",
            "local value_102 = 102",
            "local value_103 = 103",
            "local value_104 = 104",
            "local value_105 = 105",
            "local value_106 = 106",
            "local value_107 = 107",
            "local value_108 = 108",
            "local value_109 = 109",
            "local value_110 = 110",
            "local value_111 = 111",
            "local value_112 = 112",
            "local value_113 = 113",
            "local value_114 = 114",
            "local value_115 = 115",
            "local value_116 = 116",
            "local value_117 = 117",
            "local value_118 = 118",
            "local value_119 = 119",
            "local value_120 = 120",
            "local value_121 = 121",
            "local value_122 = 122",
            "local value_123 = 123",
            "local value_124 = 124",
            "local value_125 = 125",
            "local value_126 = 126",
            "local value_127 = 127",
            "local value_128 = 128",
            "local value_129 = 129",
            "local value_130 = 130",
            "local value_131 = 131",
            "local value_132 = 132",
            "local value_133 = 133",
            "local value_134 = 134",
            "local value_135 = 135",
            "local value_136 = 136",
            "local value_137 = 137",
            "local value_138 = 138",
            "local value_139 = 139",
            "local value_140 = 140",
            "local value_141 = 141",
            "local value_142 = 142",
            "local value_143 = 143",
            "local value_144 = 144",
            "local value_145 = 145",
            "local value_146 = 146",
            "local value_147 = 147",
            "local value_148 = 148",
            "local value_149 = 149",
            "local value_150 = 150",
            "local value_151 = 151",
            "local value_152 = 152",
            "local value_153 = 153",
            "local value_154 = 154",
            "local value_155 = 155",
            "local value_156 = 156",
            "local value_157 = 157",
            "local value_158 = 158",
            "local value_159 = 159",
            "local value_160 = 160",
            "local value_161 = 161",
            "local value_162 = 162",
            "local value_163 = 163",
            "local value_164 = 164",
            "local value_165 = 165",
            "local value_166 = 166",
            "local value_167 = 167",
            "local value_168 = 168",
            "local value_169 = 169",
            "local value_170 = 170",
            "local value_171 = 171",
            "local value_172 = 172",
            "local value_173 = 173",
            "local value_174 = 174",
            "local value_175 = 175",
            "local value_176 = 176",
            "local value_177 = 177",
            "local value_178 = 178",
            "local value_179 = 179",
            "local value_180 = 180",
            "local value_181 = 181",
            "local value_182 = 182",
            "local value_183 = 183",
            "local value_184 = 184",
            "local value_185 = 185",
            "local value_186 = 186",
            "local value_187 = 187",
            "local value_188 = 188",
            "local value_189 = 189",
            "local value_190 = 190",
            "local value_191 = 191",
            "local value_192 = 192",
            "local value_193 = 193",
            "local value_194 = 194",
            "local value_195 = 195",
            "local value_196 = 196",
            "local value_197 = 197",
            "local value_198 = 198",
            "local value_199 = 199",
            "local value_200 = 200",
            "local value_201 = 201",
            "local value_202 = 202",
            "local value_203 = 203",
            "local value_204 = 204",
            "local value_205 = 205",
            "local value_206 = 206",
            "local value_207 = 207",
            "local value_208 = 208",
            "local value_209 = 209",
            "local value_210 = 210",
            "local value_211 = 211",
            "local value_212 = 212",
            "local value_213 = 213",
            "local value_214 = 214",
            "local value_215 = 215",
            "local value_216 = 216",
            "local value_217 = 217",
            "local value_218 = 218",
            "local value_219 = 219",
            "local value_220 = 220",
            "local value_221 = 221",
            "local value_222 = 222",
            "local value_223 = 223",
            "local value_224 = 224",
            "local value_225 = 225",
            "local value_226 = 226",
            "local value_227 = 227",
            "local value_228 = 228",
            "local value_229 = 229",
            "local value_230 = 230",
            "local value_231 = 231",
            "local value_232 = 232",
            "local value_233 = 233",
            "local value_234 = 234",
            "local value_235 = 235",
            "local value_236 = 236",
            "local value_237 = 237",
            "local value_238 = 238",
            "local value_239 = 239",
            "local value_240 = 240",
            "local value_241 = 241",
            "local value_242 = 242",
            "local value_243 = 243",
            "local value_244 = 244",
            "local value_245 = 245",
            "local value_246 = 246",
            "local value_247 = 247",
            "local value_248 = 248",
            "local value_249 = 249",
            "local value_250 = 250",
            "local value_251 = 251",
            "local value_252 = 252",
            "local value_253 = 253",
            "local value_254 = 254",
            "local value_255 = 255",
            "local value_256 = 256"
          ],
          "metadata": {
            "decoration": [],
            "target": [],
            "visible_decoration": [
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 0
                  },
                  "end": {
                    "column": 5,
                    "row": 0
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 0
                  },
                  "end": {
                    "column": 15,
                    "row": 0
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 0
                  },
                  "end": {
                    "column": 13,
                    "row": 0
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 0
                  },
                  "end": {
                    "column": 17,
                    "row": 0
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 1
                  },
                  "end": {
                    "column": 5,
                    "row": 1
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 1
                  },
                  "end": {
                    "column": 15,
                    "row": 1
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 1
                  },
                  "end": {
                    "column": 13,
                    "row": 1
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 1
                  },
                  "end": {
                    "column": 17,
                    "row": 1
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 2
                  },
                  "end": {
                    "column": 5,
                    "row": 2
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 2
                  },
                  "end": {
                    "column": 15,
                    "row": 2
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 2
                  },
                  "end": {
                    "column": 13,
                    "row": 2
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 2
                  },
                  "end": {
                    "column": 17,
                    "row": 2
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 3
                  },
                  "end": {
                    "column": 5,
                    "row": 3
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 3
                  },
                  "end": {
                    "column": 15,
                    "row": 3
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 3
                  },
                  "end": {
                    "column": 13,
                    "row": 3
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 3
                  },
                  "end": {
                    "column": 17,
                    "row": 3
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 4
                  },
                  "end": {
                    "column": 5,
                    "row": 4
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 4
                  },
                  "end": {
                    "column": 15,
                    "row": 4
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 4
                  },
                  "end": {
                    "column": 13,
                    "row": 4
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 4
                  },
                  "end": {
                    "column": 17,
                    "row": 4
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 5
                  },
                  "end": {
                    "column": 5,
                    "row": 5
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 5
                  },
                  "end": {
                    "column": 15,
                    "row": 5
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 5
                  },
                  "end": {
                    "column": 13,
                    "row": 5
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 5
                  },
                  "end": {
                    "column": 17,
                    "row": 5
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 6
                  },
                  "end": {
                    "column": 5,
                    "row": 6
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 6
                  },
                  "end": {
                    "column": 15,
                    "row": 6
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 6
                  },
                  "end": {
                    "column": 13,
                    "row": 6
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 6
                  },
                  "end": {
                    "column": 17,
                    "row": 6
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 7
                  },
                  "end": {
                    "column": 5,
                    "row": 7
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 7
                  },
                  "end": {
                    "column": 15,
                    "row": 7
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 7
                  },
                  "end": {
                    "column": 13,
                    "row": 7
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 7
                  },
                  "end": {
                    "column": 17,
                    "row": 7
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 8
                  },
                  "end": {
                    "column": 5,
                    "row": 8
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 14,
                    "row": 8
                  },
                  "end": {
                    "column": 15,
                    "row": 8
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 8
                  },
                  "end": {
                    "column": 13,
                    "row": 8
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 8
                  },
                  "end": {
                    "column": 17,
                    "row": 8
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 9
                  },
                  "end": {
                    "column": 5,
                    "row": 9
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 9
                  },
                  "end": {
                    "column": 16,
                    "row": 9
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 9
                  },
                  "end": {
                    "column": 14,
                    "row": 9
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 9
                  },
                  "end": {
                    "column": 19,
                    "row": 9
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 10
                  },
                  "end": {
                    "column": 5,
                    "row": 10
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 10
                  },
                  "end": {
                    "column": 16,
                    "row": 10
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 10
                  },
                  "end": {
                    "column": 14,
                    "row": 10
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 10
                  },
                  "end": {
                    "column": 19,
                    "row": 10
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 11
                  },
                  "end": {
                    "column": 5,
                    "row": 11
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 11
                  },
                  "end": {
                    "column": 16,
                    "row": 11
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 11
                  },
                  "end": {
                    "column": 14,
                    "row": 11
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 11
                  },
                  "end": {
                    "column": 19,
                    "row": 11
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 12
                  },
                  "end": {
                    "column": 5,
                    "row": 12
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 12
                  },
                  "end": {
                    "column": 16,
                    "row": 12
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 12
                  },
                  "end": {
                    "column": 14,
                    "row": 12
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 12
                  },
                  "end": {
                    "column": 19,
                    "row": 12
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 13
                  },
                  "end": {
                    "column": 5,
                    "row": 13
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 13
                  },
                  "end": {
                    "column": 16,
                    "row": 13
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 13
                  },
                  "end": {
                    "column": 14,
                    "row": 13
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 13
                  },
                  "end": {
                    "column": 19,
                    "row": 13
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 14
                  },
                  "end": {
                    "column": 5,
                    "row": 14
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 14
                  },
                  "end": {
                    "column": 16,
                    "row": 14
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 14
                  },
                  "end": {
                    "column": 14,
                    "row": 14
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 14
                  },
                  "end": {
                    "column": 19,
                    "row": 14
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 15
                  },
                  "end": {
                    "column": 5,
                    "row": 15
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 15
                  },
                  "end": {
                    "column": 16,
                    "row": 15
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 15
                  },
                  "end": {
                    "column": 14,
                    "row": 15
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 15
                  },
                  "end": {
                    "column": 19,
                    "row": 15
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 16
                  },
                  "end": {
                    "column": 5,
                    "row": 16
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 16
                  },
                  "end": {
                    "column": 16,
                    "row": 16
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 16
                  },
                  "end": {
                    "column": 14,
                    "row": 16
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 16
                  },
                  "end": {
                    "column": 19,
                    "row": 16
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 17
                  },
                  "end": {
                    "column": 5,
                    "row": 17
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 17
                  },
                  "end": {
                    "column": 16,
                    "row": 17
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 17
                  },
                  "end": {
                    "column": 14,
                    "row": 17
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 17
                  },
                  "end": {
                    "column": 19,
                    "row": 17
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 18
                  },
                  "end": {
                    "column": 5,
                    "row": 18
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 18
                  },
                  "end": {
                    "column": 16,
                    "row": 18
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 18
                  },
                  "end": {
                    "column": 14,
                    "row": 18
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 18
                  },
                  "end": {
                    "column": 19,
                    "row": 18
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 19
                  },
                  "end": {
                    "column": 5,
                    "row": 19
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 19
                  },
                  "end": {
                    "column": 16,
                    "row": 19
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 19
                  },
                  "end": {
                    "column": 14,
                    "row": 19
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 19
                  },
                  "end": {
                    "column": 19,
                    "row": 19
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 20
                  },
                  "end": {
                    "column": 5,
                    "row": 20
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 20
                  },
                  "end": {
                    "column": 16,
                    "row": 20
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 20
                  },
                  "end": {
                    "column": 14,
                    "row": 20
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 20
                  },
                  "end": {
                    "column": 19,
                    "row": 20
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 21
                  },
                  "end": {
                    "column": 5,
                    "row": 21
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 21
                  },
                  "end": {
                    "column": 16,
                    "row": 21
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 21
                  },
                  "end": {
                    "column": 14,
                    "row": 21
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 21
                  },
                  "end": {
                    "column": 19,
                    "row": 21
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 22
                  },
                  "end": {
                    "column": 5,
                    "row": 22
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 22
                  },
                  "end": {
                    "column": 16,
                    "row": 22
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 22
                  },
                  "end": {
                    "column": 14,
                    "row": 22
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 22
                  },
                  "end": {
                    "column": 19,
                    "row": 22
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 23
                  },
                  "end": {
                    "column": 5,
                    "row": 23
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 23
                  },
                  "end": {
                    "column": 16,
                    "row": 23
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 23
                  },
                  "end": {
                    "column": 14,
                    "row": 23
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 23
                  },
                  "end": {
                    "column": 19,
                    "row": 23
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 24
                  },
                  "end": {
                    "column": 5,
                    "row": 24
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 24
                  },
                  "end": {
                    "column": 16,
                    "row": 24
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 24
                  },
                  "end": {
                    "column": 14,
                    "row": 24
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 24
                  },
                  "end": {
                    "column": 19,
                    "row": 24
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 25
                  },
                  "end": {
                    "column": 5,
                    "row": 25
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 25
                  },
                  "end": {
                    "column": 16,
                    "row": 25
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 25
                  },
                  "end": {
                    "column": 14,
                    "row": 25
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 25
                  },
                  "end": {
                    "column": 19,
                    "row": 25
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 26
                  },
                  "end": {
                    "column": 5,
                    "row": 26
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 26
                  },
                  "end": {
                    "column": 16,
                    "row": 26
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 26
                  },
                  "end": {
                    "column": 14,
                    "row": 26
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 26
                  },
                  "end": {
                    "column": 19,
                    "row": 26
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 27
                  },
                  "end": {
                    "column": 5,
                    "row": 27
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 27
                  },
                  "end": {
                    "column": 16,
                    "row": 27
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 27
                  },
                  "end": {
                    "column": 14,
                    "row": 27
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 27
                  },
                  "end": {
                    "column": 19,
                    "row": 27
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 28
                  },
                  "end": {
                    "column": 5,
                    "row": 28
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 28
                  },
                  "end": {
                    "column": 16,
                    "row": 28
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 28
                  },
                  "end": {
                    "column": 14,
                    "row": 28
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 28
                  },
                  "end": {
                    "column": 19,
                    "row": 28
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 29
                  },
                  "end": {
                    "column": 5,
                    "row": 29
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 29
                  },
                  "end": {
                    "column": 16,
                    "row": 29
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 29
                  },
                  "end": {
                    "column": 14,
                    "row": 29
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 29
                  },
                  "end": {
                    "column": 19,
                    "row": 29
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 30
                  },
                  "end": {
                    "column": 5,
                    "row": 30
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 30
                  },
                  "end": {
                    "column": 16,
                    "row": 30
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 30
                  },
                  "end": {
                    "column": 14,
                    "row": 30
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 30
                  },
                  "end": {
                    "column": 19,
                    "row": 30
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 31
                  },
                  "end": {
                    "column": 5,
                    "row": 31
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 31
                  },
                  "end": {
                    "column": 16,
                    "row": 31
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 31
                  },
                  "end": {
                    "column": 14,
                    "row": 31
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 31
                  },
                  "end": {
                    "column": 19,
                    "row": 31
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 32
                  },
                  "end": {
                    "column": 5,
                    "row": 32
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 32
                  },
                  "end": {
                    "column": 16,
                    "row": 32
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 32
                  },
                  "end": {
                    "column": 14,
                    "row": 32
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 32
                  },
                  "end": {
                    "column": 19,
                    "row": 32
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 33
                  },
                  "end": {
                    "column": 5,
                    "row": 33
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 33
                  },
                  "end": {
                    "column": 16,
                    "row": 33
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 33
                  },
                  "end": {
                    "column": 14,
                    "row": 33
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 33
                  },
                  "end": {
                    "column": 19,
                    "row": 33
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 34
                  },
                  "end": {
                    "column": 5,
                    "row": 34
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 34
                  },
                  "end": {
                    "column": 16,
                    "row": 34
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 34
                  },
                  "end": {
                    "column": 14,
                    "row": 34
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 34
                  },
                  "end": {
                    "column": 19,
                    "row": 34
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 35
                  },
                  "end": {
                    "column": 5,
                    "row": 35
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 35
                  },
                  "end": {
                    "column": 16,
                    "row": 35
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 35
                  },
                  "end": {
                    "column": 14,
                    "row": 35
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 35
                  },
                  "end": {
                    "column": 19,
                    "row": 35
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 36
                  },
                  "end": {
                    "column": 5,
                    "row": 36
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 36
                  },
                  "end": {
                    "column": 16,
                    "row": 36
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 36
                  },
                  "end": {
                    "column": 14,
                    "row": 36
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 36
                  },
                  "end": {
                    "column": 19,
                    "row": 36
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 37
                  },
                  "end": {
                    "column": 5,
                    "row": 37
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 37
                  },
                  "end": {
                    "column": 16,
                    "row": 37
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 37
                  },
                  "end": {
                    "column": 14,
                    "row": 37
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 37
                  },
                  "end": {
                    "column": 19,
                    "row": 37
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 38
                  },
                  "end": {
                    "column": 5,
                    "row": 38
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 38
                  },
                  "end": {
                    "column": 16,
                    "row": 38
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 38
                  },
                  "end": {
                    "column": 14,
                    "row": 38
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 38
                  },
                  "end": {
                    "column": 19,
                    "row": 38
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 39
                  },
                  "end": {
                    "column": 5,
                    "row": 39
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 39
                  },
                  "end": {
                    "column": 16,
                    "row": 39
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 39
                  },
                  "end": {
                    "column": 14,
                    "row": 39
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 39
                  },
                  "end": {
                    "column": 19,
                    "row": 39
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 40
                  },
                  "end": {
                    "column": 5,
                    "row": 40
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 40
                  },
                  "end": {
                    "column": 16,
                    "row": 40
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 40
                  },
                  "end": {
                    "column": 14,
                    "row": 40
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 40
                  },
                  "end": {
                    "column": 19,
                    "row": 40
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 41
                  },
                  "end": {
                    "column": 5,
                    "row": 41
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 41
                  },
                  "end": {
                    "column": 16,
                    "row": 41
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 41
                  },
                  "end": {
                    "column": 14,
                    "row": 41
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 41
                  },
                  "end": {
                    "column": 19,
                    "row": 41
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 42
                  },
                  "end": {
                    "column": 5,
                    "row": 42
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 42
                  },
                  "end": {
                    "column": 16,
                    "row": 42
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 42
                  },
                  "end": {
                    "column": 14,
                    "row": 42
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 42
                  },
                  "end": {
                    "column": 19,
                    "row": 42
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 43
                  },
                  "end": {
                    "column": 5,
                    "row": 43
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 43
                  },
                  "end": {
                    "column": 16,
                    "row": 43
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 43
                  },
                  "end": {
                    "column": 14,
                    "row": 43
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 43
                  },
                  "end": {
                    "column": 19,
                    "row": 43
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 44
                  },
                  "end": {
                    "column": 5,
                    "row": 44
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 44
                  },
                  "end": {
                    "column": 16,
                    "row": 44
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 44
                  },
                  "end": {
                    "column": 14,
                    "row": 44
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 44
                  },
                  "end": {
                    "column": 19,
                    "row": 44
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 45
                  },
                  "end": {
                    "column": 5,
                    "row": 45
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 45
                  },
                  "end": {
                    "column": 16,
                    "row": 45
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 45
                  },
                  "end": {
                    "column": 14,
                    "row": 45
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 45
                  },
                  "end": {
                    "column": 19,
                    "row": 45
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 46
                  },
                  "end": {
                    "column": 5,
                    "row": 46
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 46
                  },
                  "end": {
                    "column": 16,
                    "row": 46
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 46
                  },
                  "end": {
                    "column": 14,
                    "row": 46
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 46
                  },
                  "end": {
                    "column": 19,
                    "row": 46
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 47
                  },
                  "end": {
                    "column": 5,
                    "row": 47
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 47
                  },
                  "end": {
                    "column": 16,
                    "row": 47
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 47
                  },
                  "end": {
                    "column": 14,
                    "row": 47
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 47
                  },
                  "end": {
                    "column": 19,
                    "row": 47
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 48
                  },
                  "end": {
                    "column": 5,
                    "row": 48
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 48
                  },
                  "end": {
                    "column": 16,
                    "row": 48
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 48
                  },
                  "end": {
                    "column": 14,
                    "row": 48
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 48
                  },
                  "end": {
                    "column": 19,
                    "row": 48
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 49
                  },
                  "end": {
                    "column": 5,
                    "row": 49
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 49
                  },
                  "end": {
                    "column": 16,
                    "row": 49
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 49
                  },
                  "end": {
                    "column": 14,
                    "row": 49
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 49
                  },
                  "end": {
                    "column": 19,
                    "row": 49
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 50
                  },
                  "end": {
                    "column": 5,
                    "row": 50
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 50
                  },
                  "end": {
                    "column": 16,
                    "row": 50
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 50
                  },
                  "end": {
                    "column": 14,
                    "row": 50
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 50
                  },
                  "end": {
                    "column": 19,
                    "row": 50
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 51
                  },
                  "end": {
                    "column": 5,
                    "row": 51
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 51
                  },
                  "end": {
                    "column": 16,
                    "row": 51
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 51
                  },
                  "end": {
                    "column": 14,
                    "row": 51
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 51
                  },
                  "end": {
                    "column": 19,
                    "row": 51
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 52
                  },
                  "end": {
                    "column": 5,
                    "row": 52
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 52
                  },
                  "end": {
                    "column": 16,
                    "row": 52
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 52
                  },
                  "end": {
                    "column": 14,
                    "row": 52
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 52
                  },
                  "end": {
                    "column": 19,
                    "row": 52
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 53
                  },
                  "end": {
                    "column": 5,
                    "row": 53
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 53
                  },
                  "end": {
                    "column": 16,
                    "row": 53
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 53
                  },
                  "end": {
                    "column": 14,
                    "row": 53
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 53
                  },
                  "end": {
                    "column": 19,
                    "row": 53
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 54
                  },
                  "end": {
                    "column": 5,
                    "row": 54
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 54
                  },
                  "end": {
                    "column": 16,
                    "row": 54
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 54
                  },
                  "end": {
                    "column": 14,
                    "row": 54
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 54
                  },
                  "end": {
                    "column": 19,
                    "row": 54
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 55
                  },
                  "end": {
                    "column": 5,
                    "row": 55
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 55
                  },
                  "end": {
                    "column": 16,
                    "row": 55
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 55
                  },
                  "end": {
                    "column": 14,
                    "row": 55
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 55
                  },
                  "end": {
                    "column": 19,
                    "row": 55
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 56
                  },
                  "end": {
                    "column": 5,
                    "row": 56
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 56
                  },
                  "end": {
                    "column": 16,
                    "row": 56
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 56
                  },
                  "end": {
                    "column": 14,
                    "row": 56
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 56
                  },
                  "end": {
                    "column": 19,
                    "row": 56
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 57
                  },
                  "end": {
                    "column": 5,
                    "row": 57
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 57
                  },
                  "end": {
                    "column": 16,
                    "row": 57
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 57
                  },
                  "end": {
                    "column": 14,
                    "row": 57
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 57
                  },
                  "end": {
                    "column": 19,
                    "row": 57
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 58
                  },
                  "end": {
                    "column": 5,
                    "row": 58
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 58
                  },
                  "end": {
                    "column": 16,
                    "row": 58
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 58
                  },
                  "end": {
                    "column": 14,
                    "row": 58
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 58
                  },
                  "end": {
                    "column": 19,
                    "row": 58
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 59
                  },
                  "end": {
                    "column": 5,
                    "row": 59
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 59
                  },
                  "end": {
                    "column": 16,
                    "row": 59
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 59
                  },
                  "end": {
                    "column": 14,
                    "row": 59
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 59
                  },
                  "end": {
                    "column": 19,
                    "row": 59
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 60
                  },
                  "end": {
                    "column": 5,
                    "row": 60
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 60
                  },
                  "end": {
                    "column": 16,
                    "row": 60
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 60
                  },
                  "end": {
                    "column": 14,
                    "row": 60
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 60
                  },
                  "end": {
                    "column": 19,
                    "row": 60
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 61
                  },
                  "end": {
                    "column": 5,
                    "row": 61
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 61
                  },
                  "end": {
                    "column": 16,
                    "row": 61
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 61
                  },
                  "end": {
                    "column": 14,
                    "row": 61
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 61
                  },
                  "end": {
                    "column": 19,
                    "row": 61
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 62
                  },
                  "end": {
                    "column": 5,
                    "row": 62
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 62
                  },
                  "end": {
                    "column": 16,
                    "row": 62
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 62
                  },
                  "end": {
                    "column": 14,
                    "row": 62
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 62
                  },
                  "end": {
                    "column": 19,
                    "row": 62
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 63
                  },
                  "end": {
                    "column": 5,
                    "row": 63
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 63
                  },
                  "end": {
                    "column": 16,
                    "row": 63
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 63
                  },
                  "end": {
                    "column": 14,
                    "row": 63
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 63
                  },
                  "end": {
                    "column": 19,
                    "row": 63
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 64
                  },
                  "end": {
                    "column": 5,
                    "row": 64
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 64
                  },
                  "end": {
                    "column": 16,
                    "row": 64
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 64
                  },
                  "end": {
                    "column": 14,
                    "row": 64
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 64
                  },
                  "end": {
                    "column": 19,
                    "row": 64
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 65
                  },
                  "end": {
                    "column": 5,
                    "row": 65
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 65
                  },
                  "end": {
                    "column": 16,
                    "row": 65
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 65
                  },
                  "end": {
                    "column": 14,
                    "row": 65
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 65
                  },
                  "end": {
                    "column": 19,
                    "row": 65
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 66
                  },
                  "end": {
                    "column": 5,
                    "row": 66
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 66
                  },
                  "end": {
                    "column": 16,
                    "row": 66
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 66
                  },
                  "end": {
                    "column": 14,
                    "row": 66
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 66
                  },
                  "end": {
                    "column": 19,
                    "row": 66
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 67
                  },
                  "end": {
                    "column": 5,
                    "row": 67
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 67
                  },
                  "end": {
                    "column": 16,
                    "row": 67
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 67
                  },
                  "end": {
                    "column": 14,
                    "row": 67
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 67
                  },
                  "end": {
                    "column": 19,
                    "row": 67
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 68
                  },
                  "end": {
                    "column": 5,
                    "row": 68
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 68
                  },
                  "end": {
                    "column": 16,
                    "row": 68
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 68
                  },
                  "end": {
                    "column": 14,
                    "row": 68
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 68
                  },
                  "end": {
                    "column": 19,
                    "row": 68
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 69
                  },
                  "end": {
                    "column": 5,
                    "row": 69
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 69
                  },
                  "end": {
                    "column": 16,
                    "row": 69
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 69
                  },
                  "end": {
                    "column": 14,
                    "row": 69
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 69
                  },
                  "end": {
                    "column": 19,
                    "row": 69
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 70
                  },
                  "end": {
                    "column": 5,
                    "row": 70
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 70
                  },
                  "end": {
                    "column": 16,
                    "row": 70
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 70
                  },
                  "end": {
                    "column": 14,
                    "row": 70
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 70
                  },
                  "end": {
                    "column": 19,
                    "row": 70
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 71
                  },
                  "end": {
                    "column": 5,
                    "row": 71
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 71
                  },
                  "end": {
                    "column": 16,
                    "row": 71
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 71
                  },
                  "end": {
                    "column": 14,
                    "row": 71
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 71
                  },
                  "end": {
                    "column": 19,
                    "row": 71
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 72
                  },
                  "end": {
                    "column": 5,
                    "row": 72
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 72
                  },
                  "end": {
                    "column": 16,
                    "row": 72
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 72
                  },
                  "end": {
                    "column": 14,
                    "row": 72
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 72
                  },
                  "end": {
                    "column": 19,
                    "row": 72
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 73
                  },
                  "end": {
                    "column": 5,
                    "row": 73
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 73
                  },
                  "end": {
                    "column": 16,
                    "row": 73
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 73
                  },
                  "end": {
                    "column": 14,
                    "row": 73
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 73
                  },
                  "end": {
                    "column": 19,
                    "row": 73
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 74
                  },
                  "end": {
                    "column": 5,
                    "row": 74
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 74
                  },
                  "end": {
                    "column": 16,
                    "row": 74
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 74
                  },
                  "end": {
                    "column": 14,
                    "row": 74
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 74
                  },
                  "end": {
                    "column": 19,
                    "row": 74
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 75
                  },
                  "end": {
                    "column": 5,
                    "row": 75
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 75
                  },
                  "end": {
                    "column": 16,
                    "row": 75
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 75
                  },
                  "end": {
                    "column": 14,
                    "row": 75
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 75
                  },
                  "end": {
                    "column": 19,
                    "row": 75
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 76
                  },
                  "end": {
                    "column": 5,
                    "row": 76
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 76
                  },
                  "end": {
                    "column": 16,
                    "row": 76
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 76
                  },
                  "end": {
                    "column": 14,
                    "row": 76
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 76
                  },
                  "end": {
                    "column": 19,
                    "row": 76
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 77
                  },
                  "end": {
                    "column": 5,
                    "row": 77
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 77
                  },
                  "end": {
                    "column": 16,
                    "row": 77
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 77
                  },
                  "end": {
                    "column": 14,
                    "row": 77
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 77
                  },
                  "end": {
                    "column": 19,
                    "row": 77
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 78
                  },
                  "end": {
                    "column": 5,
                    "row": 78
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 78
                  },
                  "end": {
                    "column": 16,
                    "row": 78
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 78
                  },
                  "end": {
                    "column": 14,
                    "row": 78
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 78
                  },
                  "end": {
                    "column": 19,
                    "row": 78
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 79
                  },
                  "end": {
                    "column": 5,
                    "row": 79
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 79
                  },
                  "end": {
                    "column": 16,
                    "row": 79
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 79
                  },
                  "end": {
                    "column": 14,
                    "row": 79
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 79
                  },
                  "end": {
                    "column": 19,
                    "row": 79
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 80
                  },
                  "end": {
                    "column": 5,
                    "row": 80
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 80
                  },
                  "end": {
                    "column": 16,
                    "row": 80
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 80
                  },
                  "end": {
                    "column": 14,
                    "row": 80
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 80
                  },
                  "end": {
                    "column": 19,
                    "row": 80
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 81
                  },
                  "end": {
                    "column": 5,
                    "row": 81
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 81
                  },
                  "end": {
                    "column": 16,
                    "row": 81
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 81
                  },
                  "end": {
                    "column": 14,
                    "row": 81
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 81
                  },
                  "end": {
                    "column": 19,
                    "row": 81
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 82
                  },
                  "end": {
                    "column": 5,
                    "row": 82
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 82
                  },
                  "end": {
                    "column": 16,
                    "row": 82
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 82
                  },
                  "end": {
                    "column": 14,
                    "row": 82
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 82
                  },
                  "end": {
                    "column": 19,
                    "row": 82
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 83
                  },
                  "end": {
                    "column": 5,
                    "row": 83
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 83
                  },
                  "end": {
                    "column": 16,
                    "row": 83
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 83
                  },
                  "end": {
                    "column": 14,
                    "row": 83
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 83
                  },
                  "end": {
                    "column": 19,
                    "row": 83
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 84
                  },
                  "end": {
                    "column": 5,
                    "row": 84
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 84
                  },
                  "end": {
                    "column": 16,
                    "row": 84
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 84
                  },
                  "end": {
                    "column": 14,
                    "row": 84
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 84
                  },
                  "end": {
                    "column": 19,
                    "row": 84
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 85
                  },
                  "end": {
                    "column": 5,
                    "row": 85
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 85
                  },
                  "end": {
                    "column": 16,
                    "row": 85
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 85
                  },
                  "end": {
                    "column": 14,
                    "row": 85
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 85
                  },
                  "end": {
                    "column": 19,
                    "row": 85
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 86
                  },
                  "end": {
                    "column": 5,
                    "row": 86
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 86
                  },
                  "end": {
                    "column": 16,
                    "row": 86
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 86
                  },
                  "end": {
                    "column": 14,
                    "row": 86
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 86
                  },
                  "end": {
                    "column": 19,
                    "row": 86
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 87
                  },
                  "end": {
                    "column": 5,
                    "row": 87
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 87
                  },
                  "end": {
                    "column": 16,
                    "row": 87
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 87
                  },
                  "end": {
                    "column": 14,
                    "row": 87
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 87
                  },
                  "end": {
                    "column": 19,
                    "row": 87
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 88
                  },
                  "end": {
                    "column": 5,
                    "row": 88
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 88
                  },
                  "end": {
                    "column": 16,
                    "row": 88
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 88
                  },
                  "end": {
                    "column": 14,
                    "row": 88
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 88
                  },
                  "end": {
                    "column": 19,
                    "row": 88
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 89
                  },
                  "end": {
                    "column": 5,
                    "row": 89
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 89
                  },
                  "end": {
                    "column": 16,
                    "row": 89
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 89
                  },
                  "end": {
                    "column": 14,
                    "row": 89
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 89
                  },
                  "end": {
                    "column": 19,
                    "row": 89
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 90
                  },
                  "end": {
                    "column": 5,
                    "row": 90
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 90
                  },
                  "end": {
                    "column": 16,
                    "row": 90
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 90
                  },
                  "end": {
                    "column": 14,
                    "row": 90
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 90
                  },
                  "end": {
                    "column": 19,
                    "row": 90
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 91
                  },
                  "end": {
                    "column": 5,
                    "row": 91
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 91
                  },
                  "end": {
                    "column": 16,
                    "row": 91
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 91
                  },
                  "end": {
                    "column": 14,
                    "row": 91
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 91
                  },
                  "end": {
                    "column": 19,
                    "row": 91
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 92
                  },
                  "end": {
                    "column": 5,
                    "row": 92
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 92
                  },
                  "end": {
                    "column": 16,
                    "row": 92
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 92
                  },
                  "end": {
                    "column": 14,
                    "row": 92
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 92
                  },
                  "end": {
                    "column": 19,
                    "row": 92
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 93
                  },
                  "end": {
                    "column": 5,
                    "row": 93
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 93
                  },
                  "end": {
                    "column": 16,
                    "row": 93
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 93
                  },
                  "end": {
                    "column": 14,
                    "row": 93
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 93
                  },
                  "end": {
                    "column": 19,
                    "row": 93
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 94
                  },
                  "end": {
                    "column": 5,
                    "row": 94
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 94
                  },
                  "end": {
                    "column": 16,
                    "row": 94
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 94
                  },
                  "end": {
                    "column": 14,
                    "row": 94
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 94
                  },
                  "end": {
                    "column": 19,
                    "row": 94
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 95
                  },
                  "end": {
                    "column": 5,
                    "row": 95
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 95
                  },
                  "end": {
                    "column": 16,
                    "row": 95
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 95
                  },
                  "end": {
                    "column": 14,
                    "row": 95
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 95
                  },
                  "end": {
                    "column": 19,
                    "row": 95
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 96
                  },
                  "end": {
                    "column": 5,
                    "row": 96
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 96
                  },
                  "end": {
                    "column": 16,
                    "row": 96
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 96
                  },
                  "end": {
                    "column": 14,
                    "row": 96
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 96
                  },
                  "end": {
                    "column": 19,
                    "row": 96
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 97
                  },
                  "end": {
                    "column": 5,
                    "row": 97
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 97
                  },
                  "end": {
                    "column": 16,
                    "row": 97
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 97
                  },
                  "end": {
                    "column": 14,
                    "row": 97
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 97
                  },
                  "end": {
                    "column": 19,
                    "row": 97
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 98
                  },
                  "end": {
                    "column": 5,
                    "row": 98
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 15,
                    "row": 98
                  },
                  "end": {
                    "column": 16,
                    "row": 98
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 98
                  },
                  "end": {
                    "column": 14,
                    "row": 98
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 17,
                    "row": 98
                  },
                  "end": {
                    "column": 19,
                    "row": 98
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 99
                  },
                  "end": {
                    "column": 5,
                    "row": 99
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 99
                  },
                  "end": {
                    "column": 17,
                    "row": 99
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 99
                  },
                  "end": {
                    "column": 15,
                    "row": 99
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 99
                  },
                  "end": {
                    "column": 21,
                    "row": 99
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 100
                  },
                  "end": {
                    "column": 5,
                    "row": 100
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 100
                  },
                  "end": {
                    "column": 17,
                    "row": 100
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 100
                  },
                  "end": {
                    "column": 15,
                    "row": 100
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 100
                  },
                  "end": {
                    "column": 21,
                    "row": 100
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 101
                  },
                  "end": {
                    "column": 5,
                    "row": 101
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 101
                  },
                  "end": {
                    "column": 17,
                    "row": 101
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 101
                  },
                  "end": {
                    "column": 15,
                    "row": 101
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 101
                  },
                  "end": {
                    "column": 21,
                    "row": 101
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 102
                  },
                  "end": {
                    "column": 5,
                    "row": 102
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 102
                  },
                  "end": {
                    "column": 17,
                    "row": 102
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 102
                  },
                  "end": {
                    "column": 15,
                    "row": 102
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 102
                  },
                  "end": {
                    "column": 21,
                    "row": 102
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 103
                  },
                  "end": {
                    "column": 5,
                    "row": 103
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 103
                  },
                  "end": {
                    "column": 17,
                    "row": 103
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 103
                  },
                  "end": {
                    "column": 15,
                    "row": 103
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 103
                  },
                  "end": {
                    "column": 21,
                    "row": 103
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 104
                  },
                  "end": {
                    "column": 5,
                    "row": 104
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 104
                  },
                  "end": {
                    "column": 17,
                    "row": 104
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 104
                  },
                  "end": {
                    "column": 15,
                    "row": 104
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 104
                  },
                  "end": {
                    "column": 21,
                    "row": 104
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 105
                  },
                  "end": {
                    "column": 5,
                    "row": 105
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 105
                  },
                  "end": {
                    "column": 17,
                    "row": 105
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 105
                  },
                  "end": {
                    "column": 15,
                    "row": 105
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 105
                  },
                  "end": {
                    "column": 21,
                    "row": 105
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 106
                  },
                  "end": {
                    "column": 5,
                    "row": 106
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 106
                  },
                  "end": {
                    "column": 17,
                    "row": 106
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 106
                  },
                  "end": {
                    "column": 15,
                    "row": 106
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 106
                  },
                  "end": {
                    "column": 21,
                    "row": 106
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 107
                  },
                  "end": {
                    "column": 5,
                    "row": 107
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 107
                  },
                  "end": {
                    "column": 17,
                    "row": 107
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 107
                  },
                  "end": {
                    "column": 15,
                    "row": 107
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 107
                  },
                  "end": {
                    "column": 21,
                    "row": 107
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 108
                  },
                  "end": {
                    "column": 5,
                    "row": 108
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 108
                  },
                  "end": {
                    "column": 17,
                    "row": 108
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 108
                  },
                  "end": {
                    "column": 15,
                    "row": 108
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 108
                  },
                  "end": {
                    "column": 21,
                    "row": 108
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 109
                  },
                  "end": {
                    "column": 5,
                    "row": 109
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 109
                  },
                  "end": {
                    "column": 17,
                    "row": 109
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 109
                  },
                  "end": {
                    "column": 15,
                    "row": 109
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 109
                  },
                  "end": {
                    "column": 21,
                    "row": 109
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 110
                  },
                  "end": {
                    "column": 5,
                    "row": 110
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 110
                  },
                  "end": {
                    "column": 17,
                    "row": 110
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 110
                  },
                  "end": {
                    "column": 15,
                    "row": 110
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 110
                  },
                  "end": {
                    "column": 21,
                    "row": 110
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 111
                  },
                  "end": {
                    "column": 5,
                    "row": 111
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 111
                  },
                  "end": {
                    "column": 17,
                    "row": 111
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 111
                  },
                  "end": {
                    "column": 15,
                    "row": 111
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 111
                  },
                  "end": {
                    "column": 21,
                    "row": 111
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 112
                  },
                  "end": {
                    "column": 5,
                    "row": 112
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 112
                  },
                  "end": {
                    "column": 17,
                    "row": 112
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 112
                  },
                  "end": {
                    "column": 15,
                    "row": 112
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 112
                  },
                  "end": {
                    "column": 21,
                    "row": 112
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 113
                  },
                  "end": {
                    "column": 5,
                    "row": 113
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 113
                  },
                  "end": {
                    "column": 17,
                    "row": 113
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 113
                  },
                  "end": {
                    "column": 15,
                    "row": 113
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 113
                  },
                  "end": {
                    "column": 21,
                    "row": 113
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 114
                  },
                  "end": {
                    "column": 5,
                    "row": 114
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 114
                  },
                  "end": {
                    "column": 17,
                    "row": 114
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 114
                  },
                  "end": {
                    "column": 15,
                    "row": 114
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 114
                  },
                  "end": {
                    "column": 21,
                    "row": 114
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 115
                  },
                  "end": {
                    "column": 5,
                    "row": 115
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 115
                  },
                  "end": {
                    "column": 17,
                    "row": 115
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 115
                  },
                  "end": {
                    "column": 15,
                    "row": 115
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 115
                  },
                  "end": {
                    "column": 21,
                    "row": 115
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 116
                  },
                  "end": {
                    "column": 5,
                    "row": 116
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 116
                  },
                  "end": {
                    "column": 17,
                    "row": 116
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 116
                  },
                  "end": {
                    "column": 15,
                    "row": 116
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 116
                  },
                  "end": {
                    "column": 21,
                    "row": 116
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 117
                  },
                  "end": {
                    "column": 5,
                    "row": 117
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 117
                  },
                  "end": {
                    "column": 17,
                    "row": 117
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 117
                  },
                  "end": {
                    "column": 15,
                    "row": 117
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 117
                  },
                  "end": {
                    "column": 21,
                    "row": 117
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 118
                  },
                  "end": {
                    "column": 5,
                    "row": 118
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 118
                  },
                  "end": {
                    "column": 17,
                    "row": 118
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 118
                  },
                  "end": {
                    "column": 15,
                    "row": 118
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 118
                  },
                  "end": {
                    "column": 21,
                    "row": 118
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 119
                  },
                  "end": {
                    "column": 5,
                    "row": 119
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 119
                  },
                  "end": {
                    "column": 17,
                    "row": 119
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 119
                  },
                  "end": {
                    "column": 15,
                    "row": 119
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 119
                  },
                  "end": {
                    "column": 21,
                    "row": 119
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 120
                  },
                  "end": {
                    "column": 5,
                    "row": 120
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 120
                  },
                  "end": {
                    "column": 17,
                    "row": 120
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 120
                  },
                  "end": {
                    "column": 15,
                    "row": 120
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 120
                  },
                  "end": {
                    "column": 21,
                    "row": 120
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 121
                  },
                  "end": {
                    "column": 5,
                    "row": 121
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 121
                  },
                  "end": {
                    "column": 17,
                    "row": 121
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 121
                  },
                  "end": {
                    "column": 15,
                    "row": 121
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 121
                  },
                  "end": {
                    "column": 21,
                    "row": 121
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 122
                  },
                  "end": {
                    "column": 5,
                    "row": 122
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 122
                  },
                  "end": {
                    "column": 17,
                    "row": 122
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 122
                  },
                  "end": {
                    "column": 15,
                    "row": 122
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 122
                  },
                  "end": {
                    "column": 21,
                    "row": 122
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 123
                  },
                  "end": {
                    "column": 5,
                    "row": 123
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 123
                  },
                  "end": {
                    "column": 17,
                    "row": 123
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 123
                  },
                  "end": {
                    "column": 15,
                    "row": 123
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 123
                  },
                  "end": {
                    "column": 21,
                    "row": 123
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 124
                  },
                  "end": {
                    "column": 5,
                    "row": 124
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 124
                  },
                  "end": {
                    "column": 17,
                    "row": 124
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 124
                  },
                  "end": {
                    "column": 15,
                    "row": 124
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 124
                  },
                  "end": {
                    "column": 21,
                    "row": 124
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 125
                  },
                  "end": {
                    "column": 5,
                    "row": 125
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 125
                  },
                  "end": {
                    "column": 17,
                    "row": 125
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 125
                  },
                  "end": {
                    "column": 15,
                    "row": 125
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 125
                  },
                  "end": {
                    "column": 21,
                    "row": 125
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 126
                  },
                  "end": {
                    "column": 5,
                    "row": 126
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 126
                  },
                  "end": {
                    "column": 17,
                    "row": 126
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 126
                  },
                  "end": {
                    "column": 15,
                    "row": 126
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 126
                  },
                  "end": {
                    "column": 21,
                    "row": 126
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 127
                  },
                  "end": {
                    "column": 5,
                    "row": 127
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 127
                  },
                  "end": {
                    "column": 17,
                    "row": 127
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 127
                  },
                  "end": {
                    "column": 15,
                    "row": 127
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 127
                  },
                  "end": {
                    "column": 21,
                    "row": 127
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 128
                  },
                  "end": {
                    "column": 5,
                    "row": 128
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 128
                  },
                  "end": {
                    "column": 17,
                    "row": 128
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 128
                  },
                  "end": {
                    "column": 15,
                    "row": 128
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 128
                  },
                  "end": {
                    "column": 21,
                    "row": 128
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 129
                  },
                  "end": {
                    "column": 5,
                    "row": 129
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 129
                  },
                  "end": {
                    "column": 17,
                    "row": 129
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 129
                  },
                  "end": {
                    "column": 15,
                    "row": 129
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 129
                  },
                  "end": {
                    "column": 21,
                    "row": 129
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 130
                  },
                  "end": {
                    "column": 5,
                    "row": 130
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 130
                  },
                  "end": {
                    "column": 17,
                    "row": 130
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 130
                  },
                  "end": {
                    "column": 15,
                    "row": 130
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 130
                  },
                  "end": {
                    "column": 21,
                    "row": 130
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 131
                  },
                  "end": {
                    "column": 5,
                    "row": 131
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 131
                  },
                  "end": {
                    "column": 17,
                    "row": 131
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 131
                  },
                  "end": {
                    "column": 15,
                    "row": 131
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 131
                  },
                  "end": {
                    "column": 21,
                    "row": 131
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 132
                  },
                  "end": {
                    "column": 5,
                    "row": 132
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 132
                  },
                  "end": {
                    "column": 17,
                    "row": 132
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 132
                  },
                  "end": {
                    "column": 15,
                    "row": 132
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 132
                  },
                  "end": {
                    "column": 21,
                    "row": 132
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 133
                  },
                  "end": {
                    "column": 5,
                    "row": 133
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 133
                  },
                  "end": {
                    "column": 17,
                    "row": 133
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 133
                  },
                  "end": {
                    "column": 15,
                    "row": 133
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 133
                  },
                  "end": {
                    "column": 21,
                    "row": 133
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 134
                  },
                  "end": {
                    "column": 5,
                    "row": 134
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 134
                  },
                  "end": {
                    "column": 17,
                    "row": 134
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 134
                  },
                  "end": {
                    "column": 15,
                    "row": 134
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 134
                  },
                  "end": {
                    "column": 21,
                    "row": 134
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 135
                  },
                  "end": {
                    "column": 5,
                    "row": 135
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 135
                  },
                  "end": {
                    "column": 17,
                    "row": 135
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 135
                  },
                  "end": {
                    "column": 15,
                    "row": 135
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 135
                  },
                  "end": {
                    "column": 21,
                    "row": 135
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 136
                  },
                  "end": {
                    "column": 5,
                    "row": 136
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 136
                  },
                  "end": {
                    "column": 17,
                    "row": 136
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 136
                  },
                  "end": {
                    "column": 15,
                    "row": 136
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 136
                  },
                  "end": {
                    "column": 21,
                    "row": 136
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 137
                  },
                  "end": {
                    "column": 5,
                    "row": 137
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 137
                  },
                  "end": {
                    "column": 17,
                    "row": 137
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 137
                  },
                  "end": {
                    "column": 15,
                    "row": 137
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 137
                  },
                  "end": {
                    "column": 21,
                    "row": 137
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 138
                  },
                  "end": {
                    "column": 5,
                    "row": 138
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 138
                  },
                  "end": {
                    "column": 17,
                    "row": 138
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 138
                  },
                  "end": {
                    "column": 15,
                    "row": 138
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 138
                  },
                  "end": {
                    "column": 21,
                    "row": 138
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 139
                  },
                  "end": {
                    "column": 5,
                    "row": 139
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 139
                  },
                  "end": {
                    "column": 17,
                    "row": 139
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 139
                  },
                  "end": {
                    "column": 15,
                    "row": 139
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 139
                  },
                  "end": {
                    "column": 21,
                    "row": 139
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 140
                  },
                  "end": {
                    "column": 5,
                    "row": 140
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 140
                  },
                  "end": {
                    "column": 17,
                    "row": 140
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 140
                  },
                  "end": {
                    "column": 15,
                    "row": 140
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 140
                  },
                  "end": {
                    "column": 21,
                    "row": 140
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 141
                  },
                  "end": {
                    "column": 5,
                    "row": 141
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 141
                  },
                  "end": {
                    "column": 17,
                    "row": 141
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 141
                  },
                  "end": {
                    "column": 15,
                    "row": 141
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 141
                  },
                  "end": {
                    "column": 21,
                    "row": 141
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 142
                  },
                  "end": {
                    "column": 5,
                    "row": 142
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 142
                  },
                  "end": {
                    "column": 17,
                    "row": 142
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 142
                  },
                  "end": {
                    "column": 15,
                    "row": 142
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 142
                  },
                  "end": {
                    "column": 21,
                    "row": 142
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 143
                  },
                  "end": {
                    "column": 5,
                    "row": 143
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 143
                  },
                  "end": {
                    "column": 17,
                    "row": 143
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 143
                  },
                  "end": {
                    "column": 15,
                    "row": 143
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 143
                  },
                  "end": {
                    "column": 21,
                    "row": 143
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 144
                  },
                  "end": {
                    "column": 5,
                    "row": 144
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 144
                  },
                  "end": {
                    "column": 17,
                    "row": 144
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 144
                  },
                  "end": {
                    "column": 15,
                    "row": 144
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 144
                  },
                  "end": {
                    "column": 21,
                    "row": 144
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 145
                  },
                  "end": {
                    "column": 5,
                    "row": 145
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 145
                  },
                  "end": {
                    "column": 17,
                    "row": 145
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 145
                  },
                  "end": {
                    "column": 15,
                    "row": 145
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 145
                  },
                  "end": {
                    "column": 21,
                    "row": 145
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 146
                  },
                  "end": {
                    "column": 5,
                    "row": 146
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 146
                  },
                  "end": {
                    "column": 17,
                    "row": 146
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 146
                  },
                  "end": {
                    "column": 15,
                    "row": 146
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 146
                  },
                  "end": {
                    "column": 21,
                    "row": 146
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 147
                  },
                  "end": {
                    "column": 5,
                    "row": 147
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 147
                  },
                  "end": {
                    "column": 17,
                    "row": 147
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 147
                  },
                  "end": {
                    "column": 15,
                    "row": 147
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 147
                  },
                  "end": {
                    "column": 21,
                    "row": 147
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 148
                  },
                  "end": {
                    "column": 5,
                    "row": 148
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 148
                  },
                  "end": {
                    "column": 17,
                    "row": 148
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 148
                  },
                  "end": {
                    "column": 15,
                    "row": 148
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 148
                  },
                  "end": {
                    "column": 21,
                    "row": 148
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 149
                  },
                  "end": {
                    "column": 5,
                    "row": 149
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 149
                  },
                  "end": {
                    "column": 17,
                    "row": 149
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 149
                  },
                  "end": {
                    "column": 15,
                    "row": 149
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 149
                  },
                  "end": {
                    "column": 21,
                    "row": 149
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 150
                  },
                  "end": {
                    "column": 5,
                    "row": 150
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 150
                  },
                  "end": {
                    "column": 17,
                    "row": 150
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 150
                  },
                  "end": {
                    "column": 15,
                    "row": 150
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 150
                  },
                  "end": {
                    "column": 21,
                    "row": 150
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 151
                  },
                  "end": {
                    "column": 5,
                    "row": 151
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 151
                  },
                  "end": {
                    "column": 17,
                    "row": 151
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 151
                  },
                  "end": {
                    "column": 15,
                    "row": 151
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 151
                  },
                  "end": {
                    "column": 21,
                    "row": 151
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 152
                  },
                  "end": {
                    "column": 5,
                    "row": 152
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 152
                  },
                  "end": {
                    "column": 17,
                    "row": 152
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 152
                  },
                  "end": {
                    "column": 15,
                    "row": 152
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 152
                  },
                  "end": {
                    "column": 21,
                    "row": 152
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 153
                  },
                  "end": {
                    "column": 5,
                    "row": 153
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 153
                  },
                  "end": {
                    "column": 17,
                    "row": 153
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 153
                  },
                  "end": {
                    "column": 15,
                    "row": 153
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 153
                  },
                  "end": {
                    "column": 21,
                    "row": 153
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 154
                  },
                  "end": {
                    "column": 5,
                    "row": 154
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 154
                  },
                  "end": {
                    "column": 17,
                    "row": 154
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 154
                  },
                  "end": {
                    "column": 15,
                    "row": 154
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 154
                  },
                  "end": {
                    "column": 21,
                    "row": 154
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 155
                  },
                  "end": {
                    "column": 5,
                    "row": 155
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 155
                  },
                  "end": {
                    "column": 17,
                    "row": 155
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 155
                  },
                  "end": {
                    "column": 15,
                    "row": 155
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 155
                  },
                  "end": {
                    "column": 21,
                    "row": 155
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 156
                  },
                  "end": {
                    "column": 5,
                    "row": 156
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 156
                  },
                  "end": {
                    "column": 17,
                    "row": 156
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 156
                  },
                  "end": {
                    "column": 15,
                    "row": 156
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 156
                  },
                  "end": {
                    "column": 21,
                    "row": 156
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 157
                  },
                  "end": {
                    "column": 5,
                    "row": 157
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 157
                  },
                  "end": {
                    "column": 17,
                    "row": 157
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 157
                  },
                  "end": {
                    "column": 15,
                    "row": 157
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 157
                  },
                  "end": {
                    "column": 21,
                    "row": 157
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 158
                  },
                  "end": {
                    "column": 5,
                    "row": 158
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 158
                  },
                  "end": {
                    "column": 17,
                    "row": 158
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 158
                  },
                  "end": {
                    "column": 15,
                    "row": 158
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 158
                  },
                  "end": {
                    "column": 21,
                    "row": 158
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 159
                  },
                  "end": {
                    "column": 5,
                    "row": 159
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 159
                  },
                  "end": {
                    "column": 17,
                    "row": 159
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 159
                  },
                  "end": {
                    "column": 15,
                    "row": 159
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 159
                  },
                  "end": {
                    "column": 21,
                    "row": 159
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 160
                  },
                  "end": {
                    "column": 5,
                    "row": 160
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 160
                  },
                  "end": {
                    "column": 17,
                    "row": 160
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 160
                  },
                  "end": {
                    "column": 15,
                    "row": 160
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 160
                  },
                  "end": {
                    "column": 21,
                    "row": 160
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 161
                  },
                  "end": {
                    "column": 5,
                    "row": 161
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 161
                  },
                  "end": {
                    "column": 17,
                    "row": 161
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 161
                  },
                  "end": {
                    "column": 15,
                    "row": 161
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 161
                  },
                  "end": {
                    "column": 21,
                    "row": 161
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 162
                  },
                  "end": {
                    "column": 5,
                    "row": 162
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 162
                  },
                  "end": {
                    "column": 17,
                    "row": 162
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 162
                  },
                  "end": {
                    "column": 15,
                    "row": 162
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 162
                  },
                  "end": {
                    "column": 21,
                    "row": 162
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 163
                  },
                  "end": {
                    "column": 5,
                    "row": 163
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 163
                  },
                  "end": {
                    "column": 17,
                    "row": 163
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 163
                  },
                  "end": {
                    "column": 15,
                    "row": 163
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 163
                  },
                  "end": {
                    "column": 21,
                    "row": 163
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 164
                  },
                  "end": {
                    "column": 5,
                    "row": 164
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 164
                  },
                  "end": {
                    "column": 17,
                    "row": 164
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 164
                  },
                  "end": {
                    "column": 15,
                    "row": 164
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 164
                  },
                  "end": {
                    "column": 21,
                    "row": 164
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 165
                  },
                  "end": {
                    "column": 5,
                    "row": 165
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 165
                  },
                  "end": {
                    "column": 17,
                    "row": 165
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 165
                  },
                  "end": {
                    "column": 15,
                    "row": 165
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 165
                  },
                  "end": {
                    "column": 21,
                    "row": 165
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 166
                  },
                  "end": {
                    "column": 5,
                    "row": 166
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 166
                  },
                  "end": {
                    "column": 17,
                    "row": 166
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 166
                  },
                  "end": {
                    "column": 15,
                    "row": 166
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 166
                  },
                  "end": {
                    "column": 21,
                    "row": 166
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 167
                  },
                  "end": {
                    "column": 5,
                    "row": 167
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 167
                  },
                  "end": {
                    "column": 17,
                    "row": 167
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 167
                  },
                  "end": {
                    "column": 15,
                    "row": 167
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 167
                  },
                  "end": {
                    "column": 21,
                    "row": 167
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 168
                  },
                  "end": {
                    "column": 5,
                    "row": 168
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 168
                  },
                  "end": {
                    "column": 17,
                    "row": 168
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 168
                  },
                  "end": {
                    "column": 15,
                    "row": 168
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 168
                  },
                  "end": {
                    "column": 21,
                    "row": 168
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 169
                  },
                  "end": {
                    "column": 5,
                    "row": 169
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 169
                  },
                  "end": {
                    "column": 17,
                    "row": 169
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 169
                  },
                  "end": {
                    "column": 15,
                    "row": 169
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 169
                  },
                  "end": {
                    "column": 21,
                    "row": 169
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 170
                  },
                  "end": {
                    "column": 5,
                    "row": 170
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 170
                  },
                  "end": {
                    "column": 17,
                    "row": 170
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 170
                  },
                  "end": {
                    "column": 15,
                    "row": 170
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 170
                  },
                  "end": {
                    "column": 21,
                    "row": 170
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 171
                  },
                  "end": {
                    "column": 5,
                    "row": 171
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 171
                  },
                  "end": {
                    "column": 17,
                    "row": 171
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 171
                  },
                  "end": {
                    "column": 15,
                    "row": 171
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 171
                  },
                  "end": {
                    "column": 21,
                    "row": 171
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 172
                  },
                  "end": {
                    "column": 5,
                    "row": 172
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 172
                  },
                  "end": {
                    "column": 17,
                    "row": 172
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 172
                  },
                  "end": {
                    "column": 15,
                    "row": 172
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 172
                  },
                  "end": {
                    "column": 21,
                    "row": 172
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 173
                  },
                  "end": {
                    "column": 5,
                    "row": 173
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 173
                  },
                  "end": {
                    "column": 17,
                    "row": 173
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 173
                  },
                  "end": {
                    "column": 15,
                    "row": 173
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 173
                  },
                  "end": {
                    "column": 21,
                    "row": 173
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 174
                  },
                  "end": {
                    "column": 5,
                    "row": 174
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 174
                  },
                  "end": {
                    "column": 17,
                    "row": 174
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 174
                  },
                  "end": {
                    "column": 15,
                    "row": 174
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 174
                  },
                  "end": {
                    "column": 21,
                    "row": 174
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 175
                  },
                  "end": {
                    "column": 5,
                    "row": 175
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 175
                  },
                  "end": {
                    "column": 17,
                    "row": 175
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 175
                  },
                  "end": {
                    "column": 15,
                    "row": 175
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 175
                  },
                  "end": {
                    "column": 21,
                    "row": 175
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 176
                  },
                  "end": {
                    "column": 5,
                    "row": 176
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 176
                  },
                  "end": {
                    "column": 17,
                    "row": 176
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 176
                  },
                  "end": {
                    "column": 15,
                    "row": 176
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 176
                  },
                  "end": {
                    "column": 21,
                    "row": 176
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 177
                  },
                  "end": {
                    "column": 5,
                    "row": 177
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 177
                  },
                  "end": {
                    "column": 17,
                    "row": 177
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 177
                  },
                  "end": {
                    "column": 15,
                    "row": 177
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 177
                  },
                  "end": {
                    "column": 21,
                    "row": 177
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 178
                  },
                  "end": {
                    "column": 5,
                    "row": 178
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 178
                  },
                  "end": {
                    "column": 17,
                    "row": 178
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 178
                  },
                  "end": {
                    "column": 15,
                    "row": 178
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 178
                  },
                  "end": {
                    "column": 21,
                    "row": 178
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 179
                  },
                  "end": {
                    "column": 5,
                    "row": 179
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 179
                  },
                  "end": {
                    "column": 17,
                    "row": 179
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 179
                  },
                  "end": {
                    "column": 15,
                    "row": 179
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 179
                  },
                  "end": {
                    "column": 21,
                    "row": 179
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 180
                  },
                  "end": {
                    "column": 5,
                    "row": 180
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 180
                  },
                  "end": {
                    "column": 17,
                    "row": 180
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 180
                  },
                  "end": {
                    "column": 15,
                    "row": 180
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 180
                  },
                  "end": {
                    "column": 21,
                    "row": 180
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 181
                  },
                  "end": {
                    "column": 5,
                    "row": 181
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 181
                  },
                  "end": {
                    "column": 17,
                    "row": 181
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 181
                  },
                  "end": {
                    "column": 15,
                    "row": 181
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 181
                  },
                  "end": {
                    "column": 21,
                    "row": 181
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 182
                  },
                  "end": {
                    "column": 5,
                    "row": 182
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 182
                  },
                  "end": {
                    "column": 17,
                    "row": 182
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 182
                  },
                  "end": {
                    "column": 15,
                    "row": 182
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 182
                  },
                  "end": {
                    "column": 21,
                    "row": 182
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 183
                  },
                  "end": {
                    "column": 5,
                    "row": 183
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 183
                  },
                  "end": {
                    "column": 17,
                    "row": 183
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 183
                  },
                  "end": {
                    "column": 15,
                    "row": 183
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 183
                  },
                  "end": {
                    "column": 21,
                    "row": 183
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 184
                  },
                  "end": {
                    "column": 5,
                    "row": 184
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 184
                  },
                  "end": {
                    "column": 17,
                    "row": 184
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 184
                  },
                  "end": {
                    "column": 15,
                    "row": 184
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 184
                  },
                  "end": {
                    "column": 21,
                    "row": 184
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 185
                  },
                  "end": {
                    "column": 5,
                    "row": 185
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 185
                  },
                  "end": {
                    "column": 17,
                    "row": 185
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 185
                  },
                  "end": {
                    "column": 15,
                    "row": 185
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 185
                  },
                  "end": {
                    "column": 21,
                    "row": 185
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 186
                  },
                  "end": {
                    "column": 5,
                    "row": 186
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 186
                  },
                  "end": {
                    "column": 17,
                    "row": 186
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 186
                  },
                  "end": {
                    "column": 15,
                    "row": 186
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 186
                  },
                  "end": {
                    "column": 21,
                    "row": 186
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 187
                  },
                  "end": {
                    "column": 5,
                    "row": 187
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 187
                  },
                  "end": {
                    "column": 17,
                    "row": 187
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 187
                  },
                  "end": {
                    "column": 15,
                    "row": 187
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 187
                  },
                  "end": {
                    "column": 21,
                    "row": 187
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 188
                  },
                  "end": {
                    "column": 5,
                    "row": 188
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 188
                  },
                  "end": {
                    "column": 17,
                    "row": 188
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 188
                  },
                  "end": {
                    "column": 15,
                    "row": 188
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 188
                  },
                  "end": {
                    "column": 21,
                    "row": 188
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 189
                  },
                  "end": {
                    "column": 5,
                    "row": 189
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 189
                  },
                  "end": {
                    "column": 17,
                    "row": 189
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 189
                  },
                  "end": {
                    "column": 15,
                    "row": 189
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 189
                  },
                  "end": {
                    "column": 21,
                    "row": 189
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 190
                  },
                  "end": {
                    "column": 5,
                    "row": 190
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 190
                  },
                  "end": {
                    "column": 17,
                    "row": 190
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 190
                  },
                  "end": {
                    "column": 15,
                    "row": 190
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 190
                  },
                  "end": {
                    "column": 21,
                    "row": 190
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 191
                  },
                  "end": {
                    "column": 5,
                    "row": 191
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 191
                  },
                  "end": {
                    "column": 17,
                    "row": 191
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 191
                  },
                  "end": {
                    "column": 15,
                    "row": 191
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 191
                  },
                  "end": {
                    "column": 21,
                    "row": 191
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 192
                  },
                  "end": {
                    "column": 5,
                    "row": 192
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 192
                  },
                  "end": {
                    "column": 17,
                    "row": 192
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 192
                  },
                  "end": {
                    "column": 15,
                    "row": 192
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 192
                  },
                  "end": {
                    "column": 21,
                    "row": 192
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 193
                  },
                  "end": {
                    "column": 5,
                    "row": 193
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 193
                  },
                  "end": {
                    "column": 17,
                    "row": 193
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 193
                  },
                  "end": {
                    "column": 15,
                    "row": 193
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 193
                  },
                  "end": {
                    "column": 21,
                    "row": 193
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 194
                  },
                  "end": {
                    "column": 5,
                    "row": 194
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 194
                  },
                  "end": {
                    "column": 17,
                    "row": 194
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 194
                  },
                  "end": {
                    "column": 15,
                    "row": 194
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 194
                  },
                  "end": {
                    "column": 21,
                    "row": 194
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 195
                  },
                  "end": {
                    "column": 5,
                    "row": 195
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 195
                  },
                  "end": {
                    "column": 17,
                    "row": 195
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 195
                  },
                  "end": {
                    "column": 15,
                    "row": 195
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 195
                  },
                  "end": {
                    "column": 21,
                    "row": 195
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 196
                  },
                  "end": {
                    "column": 5,
                    "row": 196
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 196
                  },
                  "end": {
                    "column": 17,
                    "row": 196
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 196
                  },
                  "end": {
                    "column": 15,
                    "row": 196
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 196
                  },
                  "end": {
                    "column": 21,
                    "row": 196
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 197
                  },
                  "end": {
                    "column": 5,
                    "row": 197
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 197
                  },
                  "end": {
                    "column": 17,
                    "row": 197
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 197
                  },
                  "end": {
                    "column": 15,
                    "row": 197
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 197
                  },
                  "end": {
                    "column": 21,
                    "row": 197
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 198
                  },
                  "end": {
                    "column": 5,
                    "row": 198
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 198
                  },
                  "end": {
                    "column": 17,
                    "row": 198
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 198
                  },
                  "end": {
                    "column": 15,
                    "row": 198
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 198
                  },
                  "end": {
                    "column": 21,
                    "row": 198
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 199
                  },
                  "end": {
                    "column": 5,
                    "row": 199
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 199
                  },
                  "end": {
                    "column": 17,
                    "row": 199
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 199
                  },
                  "end": {
                    "column": 15,
                    "row": 199
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 199
                  },
                  "end": {
                    "column": 21,
                    "row": 199
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 200
                  },
                  "end": {
                    "column": 5,
                    "row": 200
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 200
                  },
                  "end": {
                    "column": 17,
                    "row": 200
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 200
                  },
                  "end": {
                    "column": 15,
                    "row": 200
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 200
                  },
                  "end": {
                    "column": 21,
                    "row": 200
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 201
                  },
                  "end": {
                    "column": 5,
                    "row": 201
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 201
                  },
                  "end": {
                    "column": 17,
                    "row": 201
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 201
                  },
                  "end": {
                    "column": 15,
                    "row": 201
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 201
                  },
                  "end": {
                    "column": 21,
                    "row": 201
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 202
                  },
                  "end": {
                    "column": 5,
                    "row": 202
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 202
                  },
                  "end": {
                    "column": 17,
                    "row": 202
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 202
                  },
                  "end": {
                    "column": 15,
                    "row": 202
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 202
                  },
                  "end": {
                    "column": 21,
                    "row": 202
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 203
                  },
                  "end": {
                    "column": 5,
                    "row": 203
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 203
                  },
                  "end": {
                    "column": 17,
                    "row": 203
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 203
                  },
                  "end": {
                    "column": 15,
                    "row": 203
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 203
                  },
                  "end": {
                    "column": 21,
                    "row": 203
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 204
                  },
                  "end": {
                    "column": 5,
                    "row": 204
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 204
                  },
                  "end": {
                    "column": 17,
                    "row": 204
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 204
                  },
                  "end": {
                    "column": 15,
                    "row": 204
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 204
                  },
                  "end": {
                    "column": 21,
                    "row": 204
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 205
                  },
                  "end": {
                    "column": 5,
                    "row": 205
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 205
                  },
                  "end": {
                    "column": 17,
                    "row": 205
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 205
                  },
                  "end": {
                    "column": 15,
                    "row": 205
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 205
                  },
                  "end": {
                    "column": 21,
                    "row": 205
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 206
                  },
                  "end": {
                    "column": 5,
                    "row": 206
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 206
                  },
                  "end": {
                    "column": 17,
                    "row": 206
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 206
                  },
                  "end": {
                    "column": 15,
                    "row": 206
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 206
                  },
                  "end": {
                    "column": 21,
                    "row": 206
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 207
                  },
                  "end": {
                    "column": 5,
                    "row": 207
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 207
                  },
                  "end": {
                    "column": 17,
                    "row": 207
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 207
                  },
                  "end": {
                    "column": 15,
                    "row": 207
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 207
                  },
                  "end": {
                    "column": 21,
                    "row": 207
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 208
                  },
                  "end": {
                    "column": 5,
                    "row": 208
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 208
                  },
                  "end": {
                    "column": 17,
                    "row": 208
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 208
                  },
                  "end": {
                    "column": 15,
                    "row": 208
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 208
                  },
                  "end": {
                    "column": 21,
                    "row": 208
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 209
                  },
                  "end": {
                    "column": 5,
                    "row": 209
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 209
                  },
                  "end": {
                    "column": 17,
                    "row": 209
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 209
                  },
                  "end": {
                    "column": 15,
                    "row": 209
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 209
                  },
                  "end": {
                    "column": 21,
                    "row": 209
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 210
                  },
                  "end": {
                    "column": 5,
                    "row": 210
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 210
                  },
                  "end": {
                    "column": 17,
                    "row": 210
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 210
                  },
                  "end": {
                    "column": 15,
                    "row": 210
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 210
                  },
                  "end": {
                    "column": 21,
                    "row": 210
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 211
                  },
                  "end": {
                    "column": 5,
                    "row": 211
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 211
                  },
                  "end": {
                    "column": 17,
                    "row": 211
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 211
                  },
                  "end": {
                    "column": 15,
                    "row": 211
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 211
                  },
                  "end": {
                    "column": 21,
                    "row": 211
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 212
                  },
                  "end": {
                    "column": 5,
                    "row": 212
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 212
                  },
                  "end": {
                    "column": 17,
                    "row": 212
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 212
                  },
                  "end": {
                    "column": 15,
                    "row": 212
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 212
                  },
                  "end": {
                    "column": 21,
                    "row": 212
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 213
                  },
                  "end": {
                    "column": 5,
                    "row": 213
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 213
                  },
                  "end": {
                    "column": 17,
                    "row": 213
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 213
                  },
                  "end": {
                    "column": 15,
                    "row": 213
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 213
                  },
                  "end": {
                    "column": 21,
                    "row": 213
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 214
                  },
                  "end": {
                    "column": 5,
                    "row": 214
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 214
                  },
                  "end": {
                    "column": 17,
                    "row": 214
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 214
                  },
                  "end": {
                    "column": 15,
                    "row": 214
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 214
                  },
                  "end": {
                    "column": 21,
                    "row": 214
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 215
                  },
                  "end": {
                    "column": 5,
                    "row": 215
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 215
                  },
                  "end": {
                    "column": 17,
                    "row": 215
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 215
                  },
                  "end": {
                    "column": 15,
                    "row": 215
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 215
                  },
                  "end": {
                    "column": 21,
                    "row": 215
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 216
                  },
                  "end": {
                    "column": 5,
                    "row": 216
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 216
                  },
                  "end": {
                    "column": 17,
                    "row": 216
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 216
                  },
                  "end": {
                    "column": 15,
                    "row": 216
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 216
                  },
                  "end": {
                    "column": 21,
                    "row": 216
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 217
                  },
                  "end": {
                    "column": 5,
                    "row": 217
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 217
                  },
                  "end": {
                    "column": 17,
                    "row": 217
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 217
                  },
                  "end": {
                    "column": 15,
                    "row": 217
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 217
                  },
                  "end": {
                    "column": 21,
                    "row": 217
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 218
                  },
                  "end": {
                    "column": 5,
                    "row": 218
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 218
                  },
                  "end": {
                    "column": 17,
                    "row": 218
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 218
                  },
                  "end": {
                    "column": 15,
                    "row": 218
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 218
                  },
                  "end": {
                    "column": 21,
                    "row": 218
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 219
                  },
                  "end": {
                    "column": 5,
                    "row": 219
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 219
                  },
                  "end": {
                    "column": 17,
                    "row": 219
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 219
                  },
                  "end": {
                    "column": 15,
                    "row": 219
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 219
                  },
                  "end": {
                    "column": 21,
                    "row": 219
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 220
                  },
                  "end": {
                    "column": 5,
                    "row": 220
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 220
                  },
                  "end": {
                    "column": 17,
                    "row": 220
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 220
                  },
                  "end": {
                    "column": 15,
                    "row": 220
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 220
                  },
                  "end": {
                    "column": 21,
                    "row": 220
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 221
                  },
                  "end": {
                    "column": 5,
                    "row": 221
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 221
                  },
                  "end": {
                    "column": 17,
                    "row": 221
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 221
                  },
                  "end": {
                    "column": 15,
                    "row": 221
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 221
                  },
                  "end": {
                    "column": 21,
                    "row": 221
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 222
                  },
                  "end": {
                    "column": 5,
                    "row": 222
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 222
                  },
                  "end": {
                    "column": 17,
                    "row": 222
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 222
                  },
                  "end": {
                    "column": 15,
                    "row": 222
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 222
                  },
                  "end": {
                    "column": 21,
                    "row": 222
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 223
                  },
                  "end": {
                    "column": 5,
                    "row": 223
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 223
                  },
                  "end": {
                    "column": 17,
                    "row": 223
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 223
                  },
                  "end": {
                    "column": 15,
                    "row": 223
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 223
                  },
                  "end": {
                    "column": 21,
                    "row": 223
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 224
                  },
                  "end": {
                    "column": 5,
                    "row": 224
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 224
                  },
                  "end": {
                    "column": 17,
                    "row": 224
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 224
                  },
                  "end": {
                    "column": 15,
                    "row": 224
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 224
                  },
                  "end": {
                    "column": 21,
                    "row": 224
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 225
                  },
                  "end": {
                    "column": 5,
                    "row": 225
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 225
                  },
                  "end": {
                    "column": 17,
                    "row": 225
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 225
                  },
                  "end": {
                    "column": 15,
                    "row": 225
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 225
                  },
                  "end": {
                    "column": 21,
                    "row": 225
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 226
                  },
                  "end": {
                    "column": 5,
                    "row": 226
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 226
                  },
                  "end": {
                    "column": 17,
                    "row": 226
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 226
                  },
                  "end": {
                    "column": 15,
                    "row": 226
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 226
                  },
                  "end": {
                    "column": 21,
                    "row": 226
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 227
                  },
                  "end": {
                    "column": 5,
                    "row": 227
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 227
                  },
                  "end": {
                    "column": 17,
                    "row": 227
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 227
                  },
                  "end": {
                    "column": 15,
                    "row": 227
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 227
                  },
                  "end": {
                    "column": 21,
                    "row": 227
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 228
                  },
                  "end": {
                    "column": 5,
                    "row": 228
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 228
                  },
                  "end": {
                    "column": 17,
                    "row": 228
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 228
                  },
                  "end": {
                    "column": 15,
                    "row": 228
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 228
                  },
                  "end": {
                    "column": 21,
                    "row": 228
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 229
                  },
                  "end": {
                    "column": 5,
                    "row": 229
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 229
                  },
                  "end": {
                    "column": 17,
                    "row": 229
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 229
                  },
                  "end": {
                    "column": 15,
                    "row": 229
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 229
                  },
                  "end": {
                    "column": 21,
                    "row": 229
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 230
                  },
                  "end": {
                    "column": 5,
                    "row": 230
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 230
                  },
                  "end": {
                    "column": 17,
                    "row": 230
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 230
                  },
                  "end": {
                    "column": 15,
                    "row": 230
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 230
                  },
                  "end": {
                    "column": 21,
                    "row": 230
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 231
                  },
                  "end": {
                    "column": 5,
                    "row": 231
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 231
                  },
                  "end": {
                    "column": 17,
                    "row": 231
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 231
                  },
                  "end": {
                    "column": 15,
                    "row": 231
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 231
                  },
                  "end": {
                    "column": 21,
                    "row": 231
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 232
                  },
                  "end": {
                    "column": 5,
                    "row": 232
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 232
                  },
                  "end": {
                    "column": 17,
                    "row": 232
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 232
                  },
                  "end": {
                    "column": 15,
                    "row": 232
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 232
                  },
                  "end": {
                    "column": 21,
                    "row": 232
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 233
                  },
                  "end": {
                    "column": 5,
                    "row": 233
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 233
                  },
                  "end": {
                    "column": 17,
                    "row": 233
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 233
                  },
                  "end": {
                    "column": 15,
                    "row": 233
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 233
                  },
                  "end": {
                    "column": 21,
                    "row": 233
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 234
                  },
                  "end": {
                    "column": 5,
                    "row": 234
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 234
                  },
                  "end": {
                    "column": 17,
                    "row": 234
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 234
                  },
                  "end": {
                    "column": 15,
                    "row": 234
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 234
                  },
                  "end": {
                    "column": 21,
                    "row": 234
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 235
                  },
                  "end": {
                    "column": 5,
                    "row": 235
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 235
                  },
                  "end": {
                    "column": 17,
                    "row": 235
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 235
                  },
                  "end": {
                    "column": 15,
                    "row": 235
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 235
                  },
                  "end": {
                    "column": 21,
                    "row": 235
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 236
                  },
                  "end": {
                    "column": 5,
                    "row": 236
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 236
                  },
                  "end": {
                    "column": 17,
                    "row": 236
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 236
                  },
                  "end": {
                    "column": 15,
                    "row": 236
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 236
                  },
                  "end": {
                    "column": 21,
                    "row": 236
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 237
                  },
                  "end": {
                    "column": 5,
                    "row": 237
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 237
                  },
                  "end": {
                    "column": 17,
                    "row": 237
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 237
                  },
                  "end": {
                    "column": 15,
                    "row": 237
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 237
                  },
                  "end": {
                    "column": 21,
                    "row": 237
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 238
                  },
                  "end": {
                    "column": 5,
                    "row": 238
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 238
                  },
                  "end": {
                    "column": 17,
                    "row": 238
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 238
                  },
                  "end": {
                    "column": 15,
                    "row": 238
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 238
                  },
                  "end": {
                    "column": 21,
                    "row": 238
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 239
                  },
                  "end": {
                    "column": 5,
                    "row": 239
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 239
                  },
                  "end": {
                    "column": 17,
                    "row": 239
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 239
                  },
                  "end": {
                    "column": 15,
                    "row": 239
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 239
                  },
                  "end": {
                    "column": 21,
                    "row": 239
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 240
                  },
                  "end": {
                    "column": 5,
                    "row": 240
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 240
                  },
                  "end": {
                    "column": 17,
                    "row": 240
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 240
                  },
                  "end": {
                    "column": 15,
                    "row": 240
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 240
                  },
                  "end": {
                    "column": 21,
                    "row": 240
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 241
                  },
                  "end": {
                    "column": 5,
                    "row": 241
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 241
                  },
                  "end": {
                    "column": 17,
                    "row": 241
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 241
                  },
                  "end": {
                    "column": 15,
                    "row": 241
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 241
                  },
                  "end": {
                    "column": 21,
                    "row": 241
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 242
                  },
                  "end": {
                    "column": 5,
                    "row": 242
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 242
                  },
                  "end": {
                    "column": 17,
                    "row": 242
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 242
                  },
                  "end": {
                    "column": 15,
                    "row": 242
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 242
                  },
                  "end": {
                    "column": 21,
                    "row": 242
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 243
                  },
                  "end": {
                    "column": 5,
                    "row": 243
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 243
                  },
                  "end": {
                    "column": 17,
                    "row": 243
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 243
                  },
                  "end": {
                    "column": 15,
                    "row": 243
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 243
                  },
                  "end": {
                    "column": 21,
                    "row": 243
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 244
                  },
                  "end": {
                    "column": 5,
                    "row": 244
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 244
                  },
                  "end": {
                    "column": 17,
                    "row": 244
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 244
                  },
                  "end": {
                    "column": 15,
                    "row": 244
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 244
                  },
                  "end": {
                    "column": 21,
                    "row": 244
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 245
                  },
                  "end": {
                    "column": 5,
                    "row": 245
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 245
                  },
                  "end": {
                    "column": 17,
                    "row": 245
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 245
                  },
                  "end": {
                    "column": 15,
                    "row": 245
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 245
                  },
                  "end": {
                    "column": 21,
                    "row": 245
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 246
                  },
                  "end": {
                    "column": 5,
                    "row": 246
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 246
                  },
                  "end": {
                    "column": 17,
                    "row": 246
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 246
                  },
                  "end": {
                    "column": 15,
                    "row": 246
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 246
                  },
                  "end": {
                    "column": 21,
                    "row": 246
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 247
                  },
                  "end": {
                    "column": 5,
                    "row": 247
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 247
                  },
                  "end": {
                    "column": 17,
                    "row": 247
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 247
                  },
                  "end": {
                    "column": 15,
                    "row": 247
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 247
                  },
                  "end": {
                    "column": 21,
                    "row": 247
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 248
                  },
                  "end": {
                    "column": 5,
                    "row": 248
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 248
                  },
                  "end": {
                    "column": 17,
                    "row": 248
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 248
                  },
                  "end": {
                    "column": 15,
                    "row": 248
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 248
                  },
                  "end": {
                    "column": 21,
                    "row": 248
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 249
                  },
                  "end": {
                    "column": 5,
                    "row": 249
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 249
                  },
                  "end": {
                    "column": 17,
                    "row": 249
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 249
                  },
                  "end": {
                    "column": 15,
                    "row": 249
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 249
                  },
                  "end": {
                    "column": 21,
                    "row": 249
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 250
                  },
                  "end": {
                    "column": 5,
                    "row": 250
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 250
                  },
                  "end": {
                    "column": 17,
                    "row": 250
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 250
                  },
                  "end": {
                    "column": 15,
                    "row": 250
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 250
                  },
                  "end": {
                    "column": 21,
                    "row": 250
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 251
                  },
                  "end": {
                    "column": 5,
                    "row": 251
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 251
                  },
                  "end": {
                    "column": 17,
                    "row": 251
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 251
                  },
                  "end": {
                    "column": 15,
                    "row": 251
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 251
                  },
                  "end": {
                    "column": 21,
                    "row": 251
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 252
                  },
                  "end": {
                    "column": 5,
                    "row": 252
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 252
                  },
                  "end": {
                    "column": 17,
                    "row": 252
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 252
                  },
                  "end": {
                    "column": 15,
                    "row": 252
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 252
                  },
                  "end": {
                    "column": 21,
                    "row": 252
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 253
                  },
                  "end": {
                    "column": 5,
                    "row": 253
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 253
                  },
                  "end": {
                    "column": 17,
                    "row": 253
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 253
                  },
                  "end": {
                    "column": 15,
                    "row": 253
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 253
                  },
                  "end": {
                    "column": 21,
                    "row": 253
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 254
                  },
                  "end": {
                    "column": 5,
                    "row": 254
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 254
                  },
                  "end": {
                    "column": 17,
                    "row": 254
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 254
                  },
                  "end": {
                    "column": 15,
                    "row": 254
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 254
                  },
                  "end": {
                    "column": 21,
                    "row": 254
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 0,
                    "row": 255
                  },
                  "end": {
                    "column": 5,
                    "row": 255
                  }
                },
                "capture": "@keyword.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 16,
                    "row": 255
                  },
                  "end": {
                    "column": 17,
                    "row": 255
                  }
                },
                "capture": "@operator.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 6,
                    "row": 255
                  },
                  "end": {
                    "column": 15,
                    "row": 255
                  }
                },
                "capture": "@variable.lua",
                "priority": 100
              },
              {
                "range": {
                  "start": {
                    "column": 18,
                    "row": 255
                  },
                  "end": {
                    "column": 21,
                    "row": 255
                  }
                },
                "capture": "@number.lua",
                "priority": 100
              }
            ],
            "editable_region": []
          },
          "id": "source:0"
        }
      ]
    },
    "title": "source file.lua @ 8440df3701c7",
    "revision": "8440df3701c7"
  },
  "id": 1
}
```

<!-- /captured-example -->

## `repository.status.context`

Resolve or act on context from a status document.

- **Context:** Repository resource; no session envelope.
- **Input:** A StatusContextRequest with the captured StatusInput and operation-specific arguments.
- **Result and effects:** Returns context choices, results, or the requested action outcome.
- **Boundary:** The captured document/view/revision determines which action is valid.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

**Current caller:** [nvim/lua/forge/source_document.lua:31](../../../../nvim/lua/forge/source_document.lua).

<!-- detailed-contract -->
### Contract

The four operations are [StatusContextRequest](native-schemas.md#statuscontextrequest). `action` requires a captured [StatusInput](repository-schemas.md#statusinput) and returns the resolved context action. `issues` requires that input plus `text` (string), finishes the resulting Git write ticket, and returns the [write outcome](#repositorywrite). `commit_message` requires a destination `document` ID, local `workspace` path, and commit `oid` string. It returns [SourceOpen](repository-schemas.md#sourceopen) containing the committed message. `about_message` requires destination `document` and `text` strings and returns SourceOpen for that immutable text.

Target resolution checks the captured status identity and freshness. Unknown context roles, changed targets, unavailable commits, and Git/write failures fail explicitly. Source results belong to their new document and must be closed through the source-document API. These operations do not generate text with a provider or emit request progress. Only the explicit issue-text action writes repository state.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 4,
  "method": "repository.status.context",
  "params": {
    "document": "example-about",
    "text": "Local fixture description",
    "operation": "about_message"
  }
}
```

```json
{
  "result": {
    "state": {
      "state": "ready"
    },
    "snapshot": {
      "document": "example-about",
      "revision": 1,
      "block": [
        {
          "id": "source:0",
          "text": [
            "Local fixture description"
          ],
          "metadata": {
            "decoration": [],
            "editable_region": [],
            "target": []
          }
        }
      ]
    },
    "title": "About",
    "more": false,
    "revision": "text",
    "object": "115b695b28f6fe0c663b87a9e98477d8b3ec4c3ecdf56b6643866d07e4d3ef92"
  },
  "id": 4
}
```

<!-- /captured-example -->

## `repository.status.document`

Open or operate on a native repository-status document.

- **Context:** Repository resource; no session envelope.
- **Input:** operation selects local, comparison, open, snapshot, refresh, demand, open_target, input, close_view, body_snapshot, or close; supply the matching typed fields.
- **Result and effects:** Returns document snapshots, patches, accepted actions, or write outcomes.
- **Boundary:** A status input action can mutate the repository; do not treat all operations as reads.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

**Current caller:** [nvim/lua/forge/status.lua:86](../../../../nvim/lua/forge/status.lua).

<!-- detailed-contract -->
### Contract

The eleven operations and their fields are enumerated in [StatusRequest](native-schemas.md#statusrequest) and the [operation tables](operation-schemas.md). Inputs use [StatusInput](repository-schemas.md#statusinput), with optional [StatusSelection](repository-schemas.md#statusselection) for `input`. `local.filename` must be absolute. `comparison.path`, when supplied, is standard base64 of repository path bytes. Other paths are ordinary local path strings.

`open`, `local`, `comparison`, and `snapshot` return [StatusSnapshot](repository-schemas.md#statussnapshot). `refresh`, `demand`, and `close_view` return native inventory/body delivery. `body_snapshot` uses the document, file ID, and captured file generation to recover a diff body. `open_target` resolves the captured target. `input` returns an accepted action, a repository [write outcome](#repositorywrite), or `{ "success": true, "patch": … }`, according to the selected action. `close` returns `{ "closed": boolean }`.

`open` emits `repository.status.document.progress` with `phase = "open"`, native timing data, and router serialization time before the final response. The document can also receive status inventory/body updates on its document subscription. Request IDs correlate timing, while document IDs, inventory revisions, file generations, and body revisions correlate content. Completion of `open` does not mean all lazily requested bodies are loaded.

Inventory and body reads invoke local Git and syntax services. Explicit input actions can stage, unstage, discard, or otherwise write repository state. Their result must be checked for per-target rejection or an unknown outcome. Invalid revisions, closed views, stale file generations, absent targets, Git failures, and source-read failures remain explicit. Retain the last usable document on failure and recover a missing patch through its snapshot operation.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [status_host.lua](../../../../nvim/tests/forge/status_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 2,
  "method": "repository.status.document",
  "params": {
    "operation": "open",
    "document": "status:51109403304300:1",
    "workspace": "/fixture/nvim.0\\KDe8Zo\\0"
  }
}
```

```json
{
  "result": {
    "file": [
      {
        "generation": 1,
        "path": "tracked file.lua",
        "change": "modified",
        "untracked": false,
        "section": "unstaged",
        "stats": {
          "added": 1,
          "state": "exact",
          "deleted": 1
        },
        "header": [
          {
            "capture": "ForgeStatusFileModified",
            "text": "Modified"
          },
          {
            "capture": "ForgeStatusPath",
            "text": " tracked file.lua"
          },
          {
            "capture": "ForgeStatusPath",
            "text": " "
          },
          {
            "capture": "ForgeAddRange",
            "text": "+1"
          },
          {
            "capture": "ForgeStatusPath",
            "text": " "
          },
          {
            "capture": "ForgeDeleteRange",
            "text": "-1"
          }
        ],
        "id": 1
      }
    ],
    "pending": [],
    "context": {
      "issues": [],
      "workspace": "/fixture/nvim.0\\KDe8Zo\\0",
      "branch_prefix": null,
      "upstream": null,
      "recent": [
        {
          "subject": "Status fixture",
          "reference": "0 seconds ago",
          "oid": "8de7e6a7ea0622bdf372745038dfa61949ee4954"
        }
      ],
      "branch": "master",
      "head": {
        "subject": "Status fixture",
        "reference": "master",
        "oid": "8de7e6a7ea0622bdf372745038dfa61949ee4954"
      },
      "push": null
    },
    "head": {
      "reference": "refs/heads/master",
      "state": "attached",
      "object": "8de7e6a7ea0622bdf372745038dfa61949ee4954"
    },
    "section": [
      {
        "kind": "unstaged",
        "file": [
          1
        ]
      }
    ],
    "document": "status:51109403304300:1",
    "revision": 0,
    "view": {
      "kind": "status"
    }
  },
  "id": 2
}
```

<!-- /captured-example -->

## `repository.status.navigate`

Resolve the adjacent status navigation target.

- **Context:** Repository resource; no session envelope.
- **Input:** input and forward.
- **Result and effects:** Returns the navigation result.
- **Boundary:** Reject stale or invalid captured input instead of choosing from a different view.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Parameter schema:** [StatusNavigationParams](native-schemas.md#statusnavigationparams).

**Current caller:** [nvim/lua/forge/status.lua:513](../../../../nvim/lua/forge/status.lua).

<!-- detailed-contract -->
### Contract

Requires `input` ([StatusInput](repository-schemas.md#statusinput)) and `forward` (boolean). The input identifies a current status document, view, sequence, and navigation origin. Returns [NavigationDelivery](repository-schemas.md#navigationdelivery): an optional cursor `effect`, optional diff `body`, and boolean `more`. A deferred destination returns body delivery with more=true so navigation can continue after applying the body. A resolved header returns a cursor effect with the captured ownership fields. End-of-range returns null effect/body with more=false. The service does not change repository state. Unknown documents, invalid view/sequence, or stale body/file identities fail. It emits no progress and invokes no provider. Apply the navigation result only if the original view still owns the cursor.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [status_host.lua](../../../../nvim/tests/forge/status_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 4,
  "method": "repository.status.navigate",
  "params": {
    "input": {
      "action": "navigate",
      "location": {
        "kind": "file",
        "id": 1
      },
      "view": "view:51109458535600:1",
      "document": "status:51109403304300:1",
      "sequence": 2,
      "revision": 0
    },
    "forward": true
  }
}
```

```json
{
  "result": {
    "body": null,
    "effect": {
      "location": {
        "file": 1,
        "position": {
          "column": 0,
          "row": 0
        },
        "block": "hunk-header:4",
        "kind": "body",
        "target": "group:5",
        "generation": 1,
        "revision": 1
      },
      "id": "navigation:6",
      "sequence": 2,
      "kind": "cursor",
      "document": "status:51109403304300:1",
      "revision": 0,
      "view": "view:51109458535600:1"
    },
    "more": false
  },
  "id": 4
}
```

<!-- /captured-example -->

## `repository.walkthrough.document`

Open or operate on a walkthrough and its annotation/source documents.

- **Context:** Repository/document resource; no session envelope.
- **Input:** operation selects open, snapshot, view, close_view, change, annotation_source, annotation_snapshot, annotation_view, annotation_close_view, annotation_close, or close.
- **Result and effects:** Returns walkthrough/source/annotation snapshots, source-row information, or close results.
- **Boundary:** Captured source/review identities and view widths are operation-specific and must remain current.
- **Inspected source:** [`nvim/rust/forge/src/walkthrough_routes.rs`](../../../rust/forge/src/walkthrough_routes.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

**Current caller:** [nvim/lua/forge/walkthrough.lua:13](../../../../nvim/lua/forge/walkthrough.lua).

<!-- detailed-contract -->
### Contract

The eleven operations and all fields are enumerated in the [operation tables](operation-schemas.md). `open` requires a unique `document` and local `workspace`; optional `inventory` defaults true. It loads the workspace's walkthrough artifact and returns its `snapshot`, `inventory_state`, optional `inventory_diagnostic`, and `stale` flag. `snapshot` returns the current [BufferSnapshot](repository-schemas.md#buffersnapshot). `view` and `close_view` return an optional [BufferPatch](repository-schemas.md#bufferpatch). Width changes reflow the retained content without changing its logical identity.

`change` requires captured `input`, a destination source `document`, and `annotation_document`. Optional `review` defaults false. It resolves the captured source and returns [WalkthroughSource](repository-schemas.md#walkthroughsource), which flattens SourceOpen and adds annotation snapshot, zero-based `source_row`, `stale`, and `review`. Review projection reads the captured before/after source. Failure closes the newly admitted annotation. `annotation_source` returns zero-based `source_row` for the captured annotation target. `annotation_snapshot`, `annotation_view`, and `annotation_close_view` return the corresponding snapshot or optional patch. `annotation_close` and `close` release their owned documents.

The service performs local artifact, Git, syntax, and optional semantic-inventory reads. It does not mutate repository files, invoke a provider, or emit request progress. Missing or invalid artifacts, stale captured actions, unavailable source, and closed document/view identities fail explicitly. Collapsed content remains in the source model while its rendered rows are omitted. Expand the relevant node before capturing a visible action, and retain document/view/revision ownership across asynchronous source resolution.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [walkthrough_host.lua](../../../../nvim/tests/forge/walkthrough_host.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "method": "repository.walkthrough.document",
  "params": {
    "workspace": "/fixture/nvim.0\\xlN92Z\\0",
    "operation": "open",
    "document": "walkthrough:47100259083900:1",
    "inventory": false
  },
  "id": 1
}
```

```json
{
  "result": {
    "stale": false,
    "snapshot": {
      "block": [
        {
          "text": [
            "Native Walkthrough"
          ],
          "metadata": {
            "target": [],
            "editable_region": [],
            "decoration": [
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 18
                  }
                },
                "priority": 110,
                "capture": "@markup.heading"
              }
            ]
          },
          "id": "walkthrough:title"
        },
        {
          "text": [
            "Overview"
          ],
          "metadata": {
            "target": [],
            "editable_region": [],
            "decoration": []
          },
          "id": "walkthrough:overview"
        },
        {
          "text": [
            "Flow",
            "Source lifecycle"
          ],
          "metadata": {
            "target": [],
            "editable_region": [],
            "decoration": [
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 4
                  }
                },
                "priority": 110,
                "capture": "@markup.heading"
              }
            ]
          },
          "id": "walkthrough:flow"
        },
        {
          "text": [
            "1. Task complete heading"
          ],
          "metadata": {
            "fold": [
              {
                "start": {
                  "row": 0,
                  "column": 0
                },
                "id": "walkthrough:task:0",
                "expand_children": false,
                "collapse_children": false,
                "end": {
                  "block": "walkthrough:task:0:subtask:0:change:0",
                  "position": {
                    "row": 3,
                    "column": 0
                  }
                },
                "closed": false
              }
            ],
            "target": [],
            "editable_region": [],
            "decoration": [
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 24
                  }
                },
                "priority": 110,
                "capture": "@markup.heading"
              },
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 24
                  }
                },
                "priority": 140,
                "capture": "ForgeWalkthroughItemTitle"
              }
            ]
          },
          "id": "walkthrough:task:0"
        },
        {
          "text": [
            "1.1. Subtask complete heading"
          ],
          "metadata": {
            "fold": [
              {
                "start": {
                  "row": 0,
                  "column": 0
                },
                "id": "walkthrough:task:0:subtask:0",
                "expand_children": false,
                "collapse_children": false,
                "end": {
                  "block": "walkthrough:task:0:subtask:0:change:0",
                  "position": {
                    "row": 3,
                    "column": 0
                  }
                },
                "closed": true
              }
            ],
            "target": [],
            "editable_region": [],
            "decoration": [
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 29
                  }
                },
                "priority": 110,
                "capture": "@markup.heading"
              },
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 29
                  }
                },
                "priority": 140,
                "capture": "ForgeWalkthroughItemTitle"
              }
            ]
          },
          "id": "walkthrough:task:0:subtask:0"
        },
        {
          "text": [
            "1.1.1 · Modify Function: captured Native source resolution",
            "",
            "source.lua:2 · open source annotation"
          ],
          "metadata": {
            "target": [
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 3,
                    "column": 0
                  }
                },
                "id": "walkthrough:task:0:subtask:0:change:0"
              }
            ],
            "editable_region": [],
            "decoration": [
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 34
                  }
                },
                "priority": 110,
                "capture": "@markup.strong"
              },
              {
                "range": {
                  "start": {
                    "row": 0,
                    "column": 0
                  },
                  "end": {
                    "row": 0,
                    "column": 59
                  }
                },
                "priority": 140,
                "capture": "ForgeWalkthroughActionModify"
              }
            ]
          },
          "id": "walkthrough:task:0:subtask:0:change:0"
        }
      ],
      "document": "walkthrough:47100259083900:1",
      "revision": 0
    },
    "inventory_diagnostic": null,
    "inventory_state": "disabled"
  },
  "id": 1
}
```

<!-- /captured-example -->

## `repository.write`

Execute the repository-write lifecycle.

- **Context:** Repository resource; no session envelope.
- **Input:** operation selects prepare, submit, cancel, acknowledge, or trace; use workspace/action or the returned intent/operation identity as required.
- **Result and effects:** Returns preparation, write outcome, acknowledgement, or trace data.
- **Boundary:** A submitted write can require outcome reconciliation; do not replay blindly.
- **Inspected source:** [`nvim/rust/forge/src/router.rs`](../../../rust/forge/src/router.rs).

**Transport:** Session ID forbidden. Readiness: `host`. Admission: `ordinary`. Recovery: `service`.

**Operation fields:** [typed discriminator and parameter tables](operation-schemas.md).

**Current caller:** [nvim/lua/forge/status.lua:591](../../../../nvim/lua/forge/status.lua).

<!-- detailed-contract -->
### Contract

The five operations and nested write actions are [WriteRequest](repository-schemas.md#writerequest) and [WriteAction](repository-schemas.md#writeaction). `prepare` requires local `workspace` and `action`, whose `kind` is `create_branch`, `commit_editor`, `push`, `publish_branch`, or `pull`. Its result contains an opaque host-lifetime `intent`, integer `operation_id`, and `timing_us` map. Preparation validates and captures repository state without submitting the write.

`submit` consumes that `intent` exactly once and returns a write outcome: integer `operation_id`, aggregate boolean `success`, per-target `target` entries, base64 `affected` paths, optional `settlement_diagnostic`, and boolean `requires_refresh`. Each target has optional base64 `path`, `completion` (`completed`, `rejected`, `outcome_unknown`, or `not_started`), optional diagnostic, and optional exit code. A successful transport response does not imply every target completed.

During submission, `repository.write.progress` events carry the request ID, `stream` (`stdout` or `stderr`), integer `sequence`, and base64 `bytes`. The terminal response follows completion of the submitted ticket. Retain the operation ID when the outcome is uncertain. `acknowledge` requires that integer operation ID and returns `{ "acknowledged": true }` only for an adoptable receipt. `cancel` requires exactly one of intent or operation ID and returns `{ "cancellation_requested": true }`. Cancellation is a request, not proof that no write happened. `trace` accepts optional integer `after` and returns `{ "host_id": string, "trace": […] }`.

Preparation tokens become invalid after host replacement or consumption. Writes execute through the shared Git mutation coordinator and can change local branches, index/worktree state, commits, or remote repositories according to the action. Git, editor, credential, stale-capture, cancellation, and settlement failures remain explicit. Do not repeat `prepare`/`submit` merely because the first callback was lost. Reconcile the retained receipt and refresh before further dependent actions.

<!-- /detailed-contract -->

<!-- captured-example -->
### Verified example

Wire frames captured from [shared_api_examples.lua](../../../../nvim/tests/forge/shared_api_examples.lua) against the rebuilt host, before Lua null normalization. Temporary paths use `/fixture`; session, resource, and document identities refer to the fixture's preceding setup.

```json
{
  "id": 5,
  "method": "repository.write",
  "params": {
    "operation": "trace"
  }
}
```

```json
{
  "result": {
    "trace": {
      "events": [],
      "sequence": 0,
      "dropped": 0
    },
    "host_id": "8484:1793252715350228000"
  },
  "id": 5
}
```

<!-- /captured-example -->

