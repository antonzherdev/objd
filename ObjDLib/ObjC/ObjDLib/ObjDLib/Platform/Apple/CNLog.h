#import "objdcore.h"
#import "ODObject.h"

#define cnLogApplyText(text) NSLog(@"%@", text)

static inline void cnLogParText(NSString* text) {
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        NSLog(@"%@", text);
    });
}

