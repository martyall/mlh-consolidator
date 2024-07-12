# MLH Consolidation

You can use this tool to

- simplify nested mlh files into a single mlh file
- convert mlh to json

## Build

```
> npm i
> npm run bundle
```

## Usage

To simplify the mlh into a single file:

```
> SIMPLIFIED_MLH=simple.mlh TOP_LEVEL_MLH=fixtures/config/lightnet.mlh node index.mjs
```
If `SIMPLIFIED_MLH` is not supplied, the tool will overwrite `TOP_LEVEL_MLH`

## Test

```
npm run test
```
