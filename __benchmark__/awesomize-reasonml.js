const R = require('ramda');
const Awesomize = require('../index');

exports.cippusFieldFactory = Awesomize.compile({
  'network_type_code': {
    read: R.path(['network_type_code']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
  'foreign_user_id': {
    read: R.path(['foreign_user_id']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
  'foreign_post_id': {
    read: R.path(['foreign_post_id']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
  'permalink': {
    read: R.path(['permalink']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
});

exports.accountFieldFactory = Awesomize.compile({
  'foreign_user_id': {
    read: R.path(['foreign_user_id']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
  'foreign_url': {
    read: R.path(['foreign_url']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
  'foreign_username': {
    read: R.path(['foreign_username']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
  'network_type_code': {
    read: R.path(['network_type_code']),
    sanitize: null,
    validate: [
      Awesomize.Validate.required,
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
  'name': {
    read: R.path(['name']),
    sanitize: null,
    validate: [
      Awesomize.Validate.isString,
    ],
    normalize: null,
  },
})
