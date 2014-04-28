#import "CNReverseLink.h"
#import "CNList.h"


@implementation CNReverseLink {

}
- (id)init {
    self = [super init];
    if (self) {

    }

    return self;
}

- (CNYield *)buildYield:(CNYield *)yield {
    __block CNImList* ret = [CNImList apply];
    return [CNYield decorateBase:yield begin:nil yield:^int(id item) {
        ret = [CNImList applyItem:item tail:ret];
        return 0;
    }                        end:^int(int result) {
        if (result != 1) {
            return [yield yieldAllItems:ret];
        }
        return result;
    }                        all:nil];
}

+ (id)link {
    return [[self alloc] init];
}

@end