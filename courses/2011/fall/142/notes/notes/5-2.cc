#include <stdio.h>
#include <string.h>

// A Mario is a
//   new_Mario ( x, y, powerup )
// where
// - x is an integer
// - y is an integer
// - powerup is a string, one of "nothing", "mushroom", "fire flower"
// (x, y) is the point between Mario's feet
// and y = 0 is on the floor

/*
 y
 ^
 |
 |
 +---> x
 */

typedef struct Mario;

typedef struct Mario {
  int x;
  int y;
  const char* powerup;
};

// Contract: new_Mario : int int string -> Mario
// Purpose: Constructs a new Mario
Mario* new_Mario( int x0, int y0, const char* powerup0 ) {
  Mario* m = new Mario();
  m->x = x0;
  m->y = y0;
  m->powerup = powerup0;
  return m;
}

// Contract: move_right : Mario int -> Mario
// Purpose: moves Mario right
Mario* move_right( Mario* m, int how_far ) {
  // Template: m, m->x, m->y, m->powerup, how_far

  //printf ( "The answer is %d, but should be %d\n",
  //         move_right( new_Mario(0, 0), 10 )->x,
  //         10 );
  //return new_Mario(10, 0);

  //printf ( "The answer is %d, but should be %d\n",
  //         move_right( new_Mario(1237, 0), 20 )->x,
  //         1257 );
  //return new_Mario(1257, 0);

  return new_Mario(m->x + how_far, m->y, m->powerup);
}

// Contract: move : Mario int int -> Mario
// Purpose: returns a moved Mario
Mario* move( Mario* m, int dx, int dy ) {
  // Template: m, m->x, m->y, m->powerup dx, dy

  //printf ( "The answer is %d, but should be %d\n",
  //         move( new_Mario(0, 0), 40, 50 )->x, 40 );
  //printf ( "The answer is %d, but should be %d\n",
  //         move( new_Mario(0, 0), 40, 50 )->y, 50 );
  //return new_Mario(40, 50);

  //printf ( "The answer is %d, but should be %d\n",
  //         move( new_Mario(1237, 523), 80, 60 )->x, 1237 + 80 );
  //printf ( "The answer is %d, but should be %d\n",
  //         move( new_Mario(1237, 523), 80, 60 )->y, 523 + 60 );
  // m = new_Mario(1237, 523)
  // m->x = 1237
  // m->y = 523
  // dx = 80
  // dy = 60
  //return new_Mario(1237 + 80, 523 + 60);

  return new_Mario(m->x + dx, m->y + dy, m->powerup);
}

// Contract: pickup : Mario string -> Mario
// Purpose: returns a new Mario with a new powerup
Mario* pickup( Mario* m, const char* pickup_powerup ) {
  // Template: m, m->x, m->y, m->powerup, new_powerup

  //printf ( "The answer is %s, but should be %s\n",
  //         pickup( new_Mario(0, 0, "nothing"), "mushroom" )->powerup,
  //         "mushroom" );
  //return new_Mario(0, 0, "mushroom");

  //printf ( "The answer is %s, but should be %s\n",
  //         pickup( new_Mario(0, 0, "nothing"), "fire flower" )->powerup,
  //         "fire flower" );
  // m = new_Mario(0, 0, "nothing")
  // new_powerup = "fire flower"
  //return new_Mario(0, 0, "fire flower");
  
  // (old powerup) (picked-up powerup) ---> new powerup
  // "nothing" "mushroom"        ---> "mushroom"
  // "nothing" "fire flower"     ---> "fire flower"
  // "mushroom" "mushroom"       ---> "mushroom"
  // "mushroom" "fire flower"    ---> "fire flower"
  // "fire flower" "mushroom"    ---> "fire flower"
  // "fire flower" "fire flower" ---> "fire flower"
  
  // Notice: the new Mario almost always gets pickup_powerup
  // He only gets something different when m->powerup = "fire flower"
  // pickup_powerup = "mushroom"
  
  if ((strcmp(m->powerup, "fire flower") == 0) &&
      (strcmp(pickup_powerup, "mushroom") == 0)) {
    return new_Mario( m->x, m->y, "fire flower");
  } else {
    return new_Mario( m->x, m->y, pickup_powerup );
  }
}

// main : -> int
int main () {
  printf ( "The answer is %d, but should be %d\n",
           new_Mario(10, 10, "nothing")->x, 10 );

  printf ( "The answer is %d, but should be %d\n",
           move_right( new_Mario(0, 0, "nothing"), 10 )->x,
           10 );
  printf ( "The answer is %d, but should be %d\n",
           move_right( new_Mario(1237, 0, "nothing"), 20 )->x,
           1257 );

  printf ( "The answer is %d, but should be %d\n",
           new_Mario(10, 0, "nothing")->x,
           10 );
  
  printf ( "The answer is %d, but should be %d\n",
           move( new_Mario(0, 0, "nothing"), 40, 50 )->x, 40 );
  printf ( "The answer is %d, but should be %d\n",
           move( new_Mario(0, 0, "nothing"), 40, 50 )->y, 50 );

  printf ( "The answer is %d, but should be %d\n",
           move( new_Mario(1237, 523, "nothing"), 80, 60 )->x, 1237 + 80 );
  printf ( "The answer is %d, but should be %d\n",
           move( new_Mario(1237, 523, "nothing"), 80, 60 )->y, 523 + 60 );
  printf ( "The answer is %s, but should be %s\n",
           move( new_Mario(1237, 523, "mushroom"), 80, 60 )->powerup, "mushroom" );
  
  printf ( "The answer is %s, but should be %s\n",
           pickup( new_Mario(0, 0, "nothing"), "mushroom" )->powerup,
           "mushroom" );
  printf ( "The answer is %s, but should be %s\n",
           pickup( new_Mario(0, 0, "nothing"), "fire flower" )->powerup,
           "fire flower" );
  printf ( "The answer is %s, but should be %s\n",
           pickup( new_Mario(0, 0, "mushroom"), "fire flower" )->powerup,
           "fire flower" );
  printf ( "The answer is %s, but should be %s\n",
           pickup( new_Mario(0, 0, "fire flower"), "mushroom" )->powerup,
           "fire flower" );

  return 0;
}
