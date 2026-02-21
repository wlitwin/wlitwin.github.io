#!/usr/bin/env node
// Entry point: node js/index.js bundle.json

const fs = require("fs");
const { loadBundle } = require("./loader");
const vm = require("./vm");

const args = process.argv.slice(2);
if (args.length === 0) {
  console.error("Usage: node js/index.js <bundle.json>");
  process.exit(1);
}

const jsonString = fs.readFileSync(args[0], "utf-8");

try {
  const result = loadBundle(jsonString);
  if (result.tag !== "unit") {
    console.log(vm.ppValue(result));
  }
} catch (e) {
  if (e instanceof vm.RuntimeError) {
    console.error(`Runtime error: ${e.message}`);
    process.exit(1);
  }
  throw e;
}
