#import "objdcore.h"
#import "CNObject.h"

#define cnLogInfoText(text) NSLog(@"%@", text)

static inline void cnLogParInfoText(NSString* text) {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        NSLog(@"%@", text);
    });
}

