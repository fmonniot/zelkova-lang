// The JavaScript companion of the `Native` facade. A facade with no companion for the
// target being built is an error (docs/spec/interop.md), and a build that checks is also
// emitted, so every package depending on `acme-widgets` needs this file to compile.
// Nothing runs it.

export function measure(size) {
  return size;
}
