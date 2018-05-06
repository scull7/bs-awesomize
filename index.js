const Awesomize = require('./src/Awesomize.bs');
const Read = require('./src/Awesomize_read.bs');
const Result = require('./src/Awesomize_result.bs');
const Scrubber = require('./src/Awesomize_data_scrub.bs');
const Validate = require('./src/Awesomize_validator.bs');

function wrap (f) {
  if (typeof f === 'function') {
    return (x) => {
      const result = f(x);
      return Promise.resolve(result);
    }
  }
  return f;
}

function translateDefinition (def) {
  return Object.assign({}, def, {
    read: wrap(def.read),
    sanitize: wrap(def.sanitize),
    normalize: wrap(def.normalize),
  });
}

exports.compile = function compile (schema) {
  let translated = Object.keys(schema).map(key => {
    return [ key, translateDefinition(schema[key]) ]
  });

  const compiledSchema = Awesomize.fromJs(translated);

  return function runSchema (input) {
    return Result.toJs(compiledSchema(input));
  };
}

Validate.extern = (fn, msg) => {
  const executor = (a, b, c) => fn(a, b, c);
  return Validate.JavaScript[1](executor, msg);
};
Validate.externDependent = Validate.JavaScript[2];

exports.make = Awesomize.make;
exports.Normalize = Scrubber;
exports.Read = Read;
exports.Result = Result;
exports.Sanitize = Scrubber;
exports.Validate = Validate;
