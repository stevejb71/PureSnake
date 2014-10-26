module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
        clean: ["gen"],
        pscMake: {
            lib: {
                src:
                    [ "purs/**/*.purs"
                    ]
            }
        },
        psc: {
            options: {},
            all : {
                src: ["purs/**/*.purs", 
                      "bower_components/purescript-arrays/src/**/*.purs",
                      "bower_components/purescript-control/src/**/*.purs",
                      "bower_components/purescript-either/src/**/*.purs",
                      "bower_components/purescript-foldable-traversable/src/**/*.purs",
                      "bower_components/purescript-foreign/src/**/*.purs",
                      "bower_components/purescript-jquery/src/**/*.purs",
                      "bower_components/purescript-monoid/src/**/*.purs",
                      "bower_components/purescript-tuples/src/**/*.purs",
                      ],
                dest: "output/snake.js"
            }
        },
        dotPsci: ["purs/**/*.purs"]
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-clean");

    grunt.registerTask("default", ["psc:all"]);
};
