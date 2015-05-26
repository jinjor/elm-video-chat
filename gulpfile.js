var gulp = require('gulp');
var plumber = require('gulp-plumber');
var watch = require('gulp-watch');
var elm = require('gulp-elm');
var cp = require('child_process');
var server = require( 'gulp-develop-server' );

gulp.task('build', function() {
  return gulp.src(['src/*.elm'])
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest('public'));

});

gulp.task('watch', function() {
  // cp.exec("node server/index", function(error, stdout, stderr) {
  //   console.log('stdout: ' + stdout);
  //   console.log('stderr: ' + stderr);
  //   if (error !== null) {
  //     console.log('exec error: ' + error);
  //   }
  // });

  server.listen( { path: 'server/index.js' } );
  gulp.watch( [ 'server/**/*.js' ], server.restart );

  gulp.watch('src/**/*.*', ['build']);

  // return gulp.src('src/*.elm')
  //   .pipe(plumber())
  //   .pipe(watch('src/*.elm'))
  //   .pipe(elm())
  //   .pipe(gulp.dest('public'));
});
