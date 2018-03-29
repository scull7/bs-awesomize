module.exports = {
  'apps': [
    {
      script: './__benchmark__/index.js',
      name: 'awebench-awesomize-benchmark',
      instances: 1,
      merge_logs: true,
      watch: true,
      ignore_watch: [
        'node_modules',
      ],
      exec_mode: 'fork',
      env: {

      },
    },
  ],
};
