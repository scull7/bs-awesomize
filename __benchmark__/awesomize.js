const R = require('ramda');
const Awesomize = require('awesomize');

function errorHandler (err) {
  throw new Error(JSON.stringify(err));
}

const awesomizer = Awesomize.dataOrError(errorHandler);

exports.cippusFieldFactory = awesomizer({}, (v) => ({
  'network_type_code': {
    read: R.path(['network_type_code']),
    validate: [ v.required, v.isString ],
  },
  'foreign_user_id': {
    read: R.path(['foreign_user_id']),
    validate: [ v.required, v.isString ],
  },
  'foreign_post_id': {
    read: R.path(['foreign_post_id']),
    validate: [ v.required, v.isString ],
  },
  'permalink': {
    read: R.path(['permalink']),
    validate: [ v.required, v.isString ],
  },
}));

exports.accountFieldFactory = awesomizer({}, (v) => ({
  'foreign_user_id': {
    read: R.path(['foreign_user_id']),
    validate: [ v.required, v.isString ],
  },
  'foreign_url': {
    read: R.path(['foreign_url']),
    validate: [ v.required, v.isString ],
  },
  'foreign_username': {
    read: R.path(['foreign_username']),
    validate: [ v.required, v.isString ],
  },
  'network_type_code': {
    read: R.path(['network_type_code']),
    validate: [ v.required, v.isString ],
  },
  'name': {
    read: R.path(['name']),
    validate: [ v.required ],
  },
}));
