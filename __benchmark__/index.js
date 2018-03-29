// @TODO - yarn add microtime
const Benchmark = require('benchmark');
const express = require('express');
const { createServer } = require('http');
const morgan = require('morgan');

const bsAwesomize = require('../index');
const reasonMLSchemas = require('./awesomize-reasonml');
const javascriptSchemas = require('./awesomize');


const cippusInputSuccess = {
  network_type_code: 'ok',
  foreign_user_id: 'ok',
  foreign_post_id: 'ok',
  permalink: 'ok',
};

function reasonMlSuccess (deferred) {
  return reasonMLSchemas.cippusFieldFactory(cippusInputSuccess)

    .then((result) => {
      if (result.awesomeResultType !== 'Ok') {
        deferred.reject(JSON.stringify(result.messages));
      }
      deferred.resolve();
    })

    .catch((err) => {
      console.error("ERROR: ", err);
      deferred.reject(JSON.stringify(err));
    });
}
function javascriptSuccess (deferred) {

  return javascriptSchemas.cippusFieldFactory(cippusInputSuccess)

    .then(() => deferred.resolve())

    .catch((err) => {
      console.error("ERROR: ", err);
      deferred.reject(JSON.stringify(err));
    });
}

function createSuite(tests) {
  const suite = new Benchmark.Suite();

  tests.forEach(([ name, fn ]) => suite.add(name, {
    'defer': true,
    'fn': fn
  }));

  return suite;
}

function createRunner(suite) {

  return (req, res, next) => {
    req.stats = [];
    suite.on('cycle', (event) => {
      req.stats.push(String(event.target));
    })
    .on('error', (...args) => console.error('ERROR: ', args))
    .on('complete', () => res.status(200).json(req.stats))
    .run({ async: true });
  }
}

const reasonMLTest = [ 'ReasonML Awesomize - success', reasonMlSuccess ];
const javascriptTest = [ 'JavaScript Awesomize - success', javascriptSuccess ];

const reasonMLSuite = createSuite([ reasonMLTest ]);
const javascriptSuite = createSuite([ javascriptTest ]);
const combinedSuite = createSuite([ reasonMLTest, javascriptTest ]);

const app = express();
app.use(morgan('dev'));
app.use('/javascript', createRunner(javascriptSuite));
app.use('/reasonml', createRunner(reasonMLSuite));
app.use('/combined', createRunner(combinedSuite));

const port = 49999
const server = createServer(app);

server.listen(port, () => {
  console.info(`Awesomize Benchmarks listening on port: ${port}`);
})
