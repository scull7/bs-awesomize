const Awesomize = require('../index');

describe('Awesomize interface to JavaScript', () => {
  const schema = Awesomize.compile({
    'foo': {
      read: ({ foo }) => foo,
      sanitize: (x) => x + 'bar',
      validate: [ Awesomize.Validate.required ],
      normalize: null,
    },
    'bar': {
      read: ({ bar }) => bar,
      sanitize: null,
      validate: [ Awesomize.Validate.required ],
      normalize: null,
    },
  });
  it('should respond with an error type', (done) => {
    schema({
      'foo': 'something',
    })
      .then((result) => {
        expect(result.awesomeResultType).toBe('Error');
        expect(result.data).toBeNull();
        expect(result.messages.foo).toBeNull();
        expect(result.messages.bar).toBe('required')
        done();
      });
  });
  it('should respond with a success type', (done) => {
    schema({
      'foo': 'thing1',
      'bar': 'thing2',
    })
      .then((result) => {
        expect(result.awesomeResultType).toBe('Ok');
        expect(result.messages).toBeNull();
        expect(result.data.foo).toBe('thing1bar');
        expect(result.data.bar).toBe('thing2');
        done();
      })
  });
});

describe('Awesomize Validator JavaScript.extern', () => {
  const greaterThanFive = (x) => x > 5;

  const validator = Awesomize.Validate.extern(
    greaterThanFive,
    "too_small"
  );

  let schema = Awesomize.compile({
    "foo": {
      read: ({ foo }) => foo,
      sanitize: null,
      validate: [ validator ],
      normalize: null,
    },
  });
  
  it("should fail when the target value is less than five", (done) => {
    schema({ foo: 2 })

      .then(result => {
        expect(result.awesomeResultType).toBe('Error');
        expect(result.data).toBeNull();
        expect(result.messages.foo).toBe("too_small");
        done();
      })
  });
  it("should pass when the target value is greater than five", (done) => {
    schema({ foo: 42 })

      .then(result => {
        expect(result.awesomeResultType).toBe('Ok');
        expect(result.messages).toBeNull();
        expect(result.data.foo).toBe(42);
        done();
      });
  });
  it("should pass when the target value is not present", (done) => {
    schema({})

      .then(result => {
        expect(result.awesomeResultType).toBe('Ok');
        expect(result.messages).toBeNull();
        expect(result.data.foo).toBeUndefined();
        done();
      });
  });
});

describe('Awesomize Validator JavaScript.externDependent', () => {
  const notEqual = (target, dependent, _sanitized) => {
    if (dependent == null) {
      return Promise.resolve(false);
    }
    return Promise.resolve(target !== dependent);
  };

  let validator =  Awesomize.Validate.externDependent(
    notEqual,
    "bar",
    "should_not_match"
  );

  const schema = Awesomize.compile({
    "foo": {
      read: ({ foo }) => foo,
      sanitize: null,
      validate: [ validator ],
      normalize: null,
    },
    "bar": {
      read: ({ bar }) => bar,
      sanitize: null,
      validate: [ Awesomize.Validate.required ],
      normalize: null,
    },
  });

  it("should fail when the dependent value is not present", (done) => {
    schema({ foo: 42 })

      .then(result => {
        expect(result.awesomeResultType).toBe('Error');
        expect(result.data).toBeNull();
        expect(result.messages.foo).toBe("should_not_match");
        expect(result.messages.bar).toBe("required");
        done();
      });
  });

  it("should fail when the dependent validator test fails", (done) => {
    schema({ foo: 42, bar: 42 })

      .then(result => {
        expect(result.awesomeResultType).toBe('Error');
        expect(result.data).toBeNull();
        expect(result.messages.foo).toBe("should_not_match");
        expect(result.messages.bar).toBeNull();
        done();
      });
  });

  it("should pass when the dependent validator test passes", (done) => {
    schema({ foo: 42, bar: 43 })

      .then((result) => {
        expect(result.awesomeResultType).toBe('Ok');
        expect(result.messages).toBeNull();
        expect(result.data.foo).toBe(42);
        expect(result.data.bar).toBe(43);
        done();
      });
  });

  describe("Synchronous validator", () => {
    const isEqual = (target, dependent, _sanitized) => (dependent == null)
      ? false
      : (target === dependent);

    const schema = Awesomize.compile({
      "foo": {
        read: ({ foo }) => foo,
        sanitize: null,
        validate: [
          Awesomize.Validate.externDependent(isEqual, "bar", "must_match"),
        ],
        normalize: null,
      },
      "bar": {
        read: ({ bar }) => bar,
        sanitize: null,
        validate: [ Awesomize.Validate.required ],
        normalize: null,
      },
    });

    it("should fail when the dependent value is not present", (done) => {
      schema({ foo: 42 })

        .then(result => {
          expect(result.awesomeResultType).toBe('Error');
          expect(result.data).toBeNull();
          expect(result.messages.foo).toBe("must_match");
          expect(result.messages.bar).toBe("required");
          done();
        });
    });

    it("should fail when the dependent validator test fails", (done) => {
      schema({ foo: 42, bar: 43 })

        .then(result => {
          expect(result.awesomeResultType).toBe('Error');
          expect(result.data).toBeNull();
          expect(result.messages.foo).toBe("must_match");
          expect(result.messages.bar).toBeNull();
          done();
        });
    });

    it("should pass when the dependent validator test passes", (done) => {
      schema({ foo: 42, bar: 42 })

        .then((result) => {
          expect(result.awesomeResultType).toBe('Ok');
          expect(result.messages).toBeNull();
          expect(result.data.foo).toBe(42);
          expect(result.data.bar).toBe(42);
          done();
        });
    });
  });
});
