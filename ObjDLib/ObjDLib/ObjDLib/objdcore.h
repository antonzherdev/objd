#import <Foundation/Foundation.h>
#import "ODType.h"

@protocol CNSeq;

static inline void delay(double seconds, dispatch_block_t action) {
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, (int64_t)(seconds * NSEC_PER_SEC));
    dispatch_after(popTime, dispatch_get_main_queue(), action);
}