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

function assertIsFunction (prop, key, def) {
  const type = typeof def[prop];

  if (type !== 'function') {
    throw new TypeError(
      `${prop} property expects 'function' received '${type}' for '${key}'.`
    );
  }
}

function assertIsFunctionOrNil (prop, key, def) {
  if (def.hasOwnProperty(prop) && def[prop] != null) {
    assertIsFunction(prop, key, def);
  }
}

function assertIsNotNil (prop, key, def) {
  if (!def.hasOwnProperty(prop) || def[prop] == null) {
    throw new TypeError(`${prop} property is required for '${key}'.`);
  }
}

function assertIsRequiredFunction (prop, key, def) {
  assertIsNotNil(prop, key, def);
  assertIsFunction(prop, key, def);
}

function assertIsRequiredArrayOfFunction (prop, key, def) {
  assertIsNotNil(prop, key, def);

  if (!Array.isArray(def[prop])) {
    throw new TypeError(`${prop} property for '${key}' must be an array.`);
  }

  def[prop].map((x, index) => {
    const type = typeof x;
    if (type !== 'function') {
      throw new TypeError(
        `${prop} property expects an Array<Function> ` +
       `received '${type}' for '${key}' at index '${index}'`
      )
    }
  });
}

function assertRequiredObject(name, target) {
  // because... "typeof null === 'object'", yay javascript!
  const type = target === null ? 'null' : typeof target;

  if (type !== 'object') {
    throw new TypeError(`${name} must be an 'object' received '${type}'.`);
  }
}

exports.compile = function compile (schema) {
  const schemaType = typeof schema;

  assertRequiredObject('schema', schema);

  let translated = Object.keys(schema).map(key => {

    const def = schema[key];

    assertRequiredObject(`definition for '${key}'`, def);
    assertIsRequiredFunction('read', key, def);
    assertIsRequiredArrayOfFunction('validate', key, def);
    assertIsFunctionOrNil('sanitize', key, def);
    assertIsFunctionOrNil('normalize', key, def);

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
