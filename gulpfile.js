var gulp        = require('gulp');
var purescript  = require('gulp-purescript');
var foreach     = require('gulp-foreach');
var plumber     = require('gulp-plumber');
var notify      = require('gulp-notify');
var browserSync = require('browser-sync');

var path       = require('path');

var bowerPurs = 'bower_components/purescript-*/src/**/*.purs';
var sources = [bowerPurs, 'src/**/*.purs', 'examples/**/*.purs'];

gulp.task('pscMake', function(){
  return gulp
    .src(sources)
    .pipe(purescript.pscMake());
});

gulp.task('dotPsci', function(){
  return gulp
    .src(sources)
    .pipe(purescript.dotPsci());
});

gulp.task('pscDocs', function(){
  return gulp
    .src('src/**/*.purs')
    .pipe(foreach(function(stream, file){
      var p = path.resolve(
        'docs',
        path.dirname(file.relative),
        path.basename(file.relative, ".purs") + ".md")
      return stream
        .pipe(purescript.pscDocs())
        .pipe(gulp.dest(p));
    }));
});

gulp.task('develop', function(){
  browserSync({server: {baseDir: "examples"}, startPath: 'index.html'});

  gulp.watch(sources, ['example']);
});

gulp.task('example', function(){
  return gulp
    .src(sources)
    .pipe(plumber({errorHandler: notify.onError("Error: <%= error.message %>")}))
    .pipe(purescript.psc(
      { main: "Main"
      , output: 'main.js'
      , modules: ['Main']
      }))
    .pipe(gulp.dest('examples/'))
    .pipe(browserSync.reload({stream: true}));
});

gulp.task('default', ['pscMake', 'dotPsci', 'pscDocs', 'example']);
