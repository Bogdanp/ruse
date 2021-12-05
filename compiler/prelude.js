let $root = scheme = {};
scheme['+'] = function(a, b) {
  return a + b;
}
scheme['-'] = function(a, b) {
  return a - b;
}
scheme['<'] = function(a, b) {
  return a < b;
}
scheme['displayln'] = console.log;
