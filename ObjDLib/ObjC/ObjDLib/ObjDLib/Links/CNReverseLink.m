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
    return [CNYield decorateBase:yield begin:nil yield:^CNYieldResult(id item) {
        ret = [CNImList applyItem:item tail:ret];
        return cnYieldContinue;
    }                        end:^CNYieldResult(CNYieldResult result) {
        if (result != cnYieldBreak) {
            return [yield yieldAll:ret];
        }
        return result;
    }                        all:nil];
}

+ (id)link {
    return [[self alloc] init];
}

@end