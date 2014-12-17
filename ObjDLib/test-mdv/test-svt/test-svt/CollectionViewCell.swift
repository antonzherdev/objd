//
//  CollectionViewCell.swift
//  test-svt
//
//  Created by Anton Zherdev on 06/06/14.
//  Copyright (c) 2014 Anton Zherdev. All rights reserved.
//

import UIKit

class CollectionViewCell: UICollectionViewCell {

    @IBOutlet var lavel : UILabel
    init(frame: CGRect) {
        super.init(frame: frame)
        // Initialization code
    }
    
    init(coder aDecoder: NSCoder!) {
        super.init(coder: aDecoder)
    }
    
}
